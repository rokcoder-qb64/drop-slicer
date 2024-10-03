'======================================================================================================================================================================================================
' Drop Slicer
'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' Programmed by RokCoder
'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' Just a game...
'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' V0.1 - 03/10/2024 - First release
'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' https://github.com/rokcoder-qb64/drop-slicer
'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' https://www.rokcoder.com
' https://www.github.com/rokcoder
' https://www.facebook.com/rokcoder
' https://www.youtube.com/rokcoder
'======================================================================================================================================================================================================
' TODO
'======================================================================================================================================================================================================

$VersionInfo:CompanyName='RokSoft'
$VersionInfo:FileDescription='QB64 Drop Slicer'
$VersionInfo:InternalName='drop-slicer.exe'
$VersionInfo:ProductName='Drop Slicer'
$VersionInfo:OriginalFilename='drop-slicer.exe'
$VersionInfo:LegalCopyright='(c)2024 RokSoft'
$VersionInfo:FILEVERSION#=0,1,0,0
$VersionInfo:PRODUCTVERSION#=0,1,0,0

'======================================================================================================================================================================================================

Option _Explicit
Option _ExplicitArray

'======================================================================================================================================================================================================

Const FALSE = 0
Const TRUE = Not FALSE

Const USE_RECTANGLES = FALSE ' Unfortunately Physac doesn't handle rectangles very well so I'm defaulting to using squares

Const SCREEN_WIDTH = 480 ' Resolution of the unscaled game area
Const SCREEN_HEIGHT = 360

Const VERSION = 1

Const STATE_NEW_GAME = 0 ' Different game states used in the game
Const STATE_GAME_OVER = 1
Const STATE_WAIT_TO_START = 2
Const STATE_WAIT_TO_DROP_BLOCK = 3
Const STATE_PLAY_TURN = 4
Const STATE_DROP_BLOCK = 5
Const STATE_WAIT_TO_LAND = 6

Const START_BUTTON_WIDTH = 231
Const START_BUTTON_HEIGHT = 87
Const START_BUTTON_Y = 200

Const BLOCK_START_WIDTH = 240
Const BLOCK_HEIGHT = 20
Const MAX_BLOCKS = 50
Const BLOCK_START_HEIGHT = SCREEN_HEIGHT - 50

Const FLOOR_HEIGHT = SCREEN_HEIGHT / 2 - 50

Const SLICE_FADE_FRAMES = 40

Const SFX_FALL = 0
Const SFX_LOSE = 1
Const SFX_LAND = 2
Const SFX_START = 4

'======================================================================================================================================================================================================

Type SCROLL
    active As Integer
    count As Integer
End Type

Type GAME
    fps As Integer
    score As Long
    hiscore As Long
    currentWidth As Integer
    blockCount As Integer
    gameOver As Integer
    scroll As SCROLL
End Type

Type VECTOR2
    x As Single
    y As Single
End Type

Type STATE
    state As Integer
    substate As Integer
    counter As Integer
End Type

Type GLDATA
    initialised As Integer
    executing As Integer
    background As Long
    normal As Long
    bubbleText As Long
End Type

Type SFX
    handle As Long
    oneShot As Integer
    looping As Integer
End Type

Type BLOCK
    positional As Integer
    position As VECTOR2
    width As Integer
    yVelocity As Single
End Type

Type slice
    block As BLOCK
    frame As Integer
    active As Integer
End Type

'======================================================================================================================================================================================================

' Not a fan of globals but this is QB64 so what can you do?

Dim Shared state As STATE
Dim Shared glData As GLDATA
Dim Shared virtualScreen& ' Handle to virtual screen which is drawn to and then blitted/stretched to the main display
Dim Shared game As GAME ' General game data
Dim Shared sfx(4) As SFX
Dim Shared quit As Integer
Dim Shared exitProgram As Integer
Dim Shared block(MAX_BLOCKS) As BLOCK
Dim Shared gravity!
Dim Shared slice As slice

'===== Game loop ======================================================================================================================================================================================

PrepareGame
Do: _Limit (game.fps%)
    UpdateFrame
    _PutImage , virtualScreen&, 0, (0, 0)-(SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1) ' Copy from virtual screen to target screen which allows for automatic upscaling
    _Display
    If exitProgram Then _FreeImage virtualScreen&: System
Loop

'===== Error handling =================================================================================================================================================================================

fileReadError:
InitialiseHiscore
Resume Next

fileWriteError:
On Error GoTo 0
Resume Next

'===== One time initialisations =======================================================================================================================================================================

Sub PrepareGame
    Dim m%
    quit = _Exit
    exitProgram = FALSE
    _DisplayOrder _Software
    m% = Int((_DesktopHeight - 80) / SCREEN_HEIGHT) ' Ratio for how much we can scale the game up (integer values) whilst still fitting vertically on the screen
    virtualScreen& = _NewImage(SCREEN_WIDTH, SCREEN_HEIGHT, 32) ' This is the same resolution as the original arcade game
    Screen _NewImage(SCREEN_WIDTH * m%, SCREEN_HEIGHT * m%, 32) ' The screen we ultimately display is the defined size multiplied by a ratio as determined above
    Do: _Delay 0.5: Loop Until _ScreenExists
    _ScreenMove _Middle
    $Resize:Stretch
    _AllowFullScreen _SquarePixels , _Smooth
    _Title "Drop Slicer"
    $ExeIcon:'./assets/icon.ico'
    _Dest virtualScreen&
    game.fps% = 60
    Randomize Timer
    glData.executing = TRUE
    _DisplayOrder _Hardware , _GLRender , _Software
    LoadAllSFX
    ReadHiscore ' Read high scores from file (or create them if the file doesn't exist or can't be read)
    SetGameState STATE_WAIT_TO_START ' Set the game state in its initial state
    gravity! = -9.82 / game.fps%
End Sub

'===== High score code ================================================================================================================================================================================

' ReadHiscores
' - Read high scores from local storage (with fallback to initialising data if there's an error while reading the file for any reason)
Sub ReadHiscore
    Dim handle&, s&, v%
    On Error GoTo fileReadError
    If Not _FileExists("scores.txt") Then InitialiseHiscore: Exit Sub
    handle& = FreeFile
    Open "scores.txt" For Input As #handle&
    Input #handle&, s&
    If EOF(handle&) Then
        ' This was a high score file containing only hard level high score (before a version number was introduced)
        game.hiscore& = 0
    Else
        v% = s&
        Input #handle&, game.hiscore&
    End If
    Close #handle&
    On Error GoTo 0
End Sub

' InitialiseHiscores
' - Set up default high score values
Sub InitialiseHiscore
    game.hiscore& = 0
End Sub

' WriteHiscores
' - Store high scores to local storage (trapping any errors that might occur - write-protected, out of space, etc)
Sub WriteHiscore
    Dim handle&
    On Error GoTo fileWriteError
    handle& = FreeFile
    Open "scores.txt" For Output As #handle&
    Print #handle&, VERSION
    Print #handle&, game.hiscore&
    Close #handle&
    On Error GoTo 0
End Sub

'===== Simple asset loading functions =================================================================================================================================================================

Sub AssetError (fname$)
    Screen 0
    Print "Unable to load "; fname$
    Print "Please make sure EXE is in same folder as drop-slicer.bas"
    Print "(Set Run/Output EXE to Source Folder option in the IDE before compiling)"
    End
End Sub

Function LoadImage& (fname$)
    Dim asset&, f$
    f$ = "./assets/" + fname$ + ".png"
    asset& = _LoadImage(f$, 32)
    If asset& = -1 Then AssetError (f$)
    LoadImage& = asset&
End Function

Function SndOpen& (fname$)
    Dim asset&, f$
    f$ = "./assets/" + fname$
    asset& = _SndOpen(f$)
    If asset& = -1 Then AssetError (f$)
    SndOpen& = asset&
End Function

'===== Sound manager ==================================================================================================================================================================================

Sub LoadSfx (sfx%, sfx$, oneShot%)
    sfx(sfx%).handle& = _SndOpen("assets/" + sfx$ + ".ogg")
    If sfx(sfx%).handle& = 0 Then AssetError sfx$
    sfx(sfx%).oneShot% = oneShot%
End Sub

Sub LoadAllSFX
    LoadSfx SFX_FALL, "fall", TRUE
    LoadSfx SFX_LAND, "land", TRUE
    LoadSfx SFX_LOSE, "lose", TRUE
    LoadSfx SFX_START, "start", TRUE
End Sub

Sub PlaySfx (sfx%)
    If sfx(sfx%).oneShot% Then
        _SndPlay sfx(sfx%).handle&
    Else
        _SndPlayCopy sfx(sfx%).handle&
    End If
End Sub

Sub PlaySfxLooping (sfx%)
    If sfx(sfx%).oneShot% Then
        _SndLoop sfx(sfx%).handle&
    End If
End Sub

Sub StopSfx (sfx%)
    If sfx(sfx%).oneShot% Then _SndStop sfx(sfx%).handle&
End Sub

Function IsPlayingSfx% (sfx%)
    IsPlayingSfx% = _SndPlaying(sfx(sfx%).handle&)
End Function

Sub SetGameState (s%)
    state.state% = s%
    state.substate% = 0
    state.counter% = 0
    If s% = STATE_NEW_GAME Then InitialiseGame: SetGameState STATE_PLAY_TURN
    If s% = STATE_PLAY_TURN Then ChooseBlockToDrop: SetGameState STATE_WAIT_TO_DROP_BLOCK
    If s% = STATE_GAME_OVER Then WriteHiscore: SetGameState STATE_WAIT_TO_START
End Sub

'======================================================================================================================================================================================================

Sub UpdateFrame
    Do While _MouseInput: Loop
    If state.state% = STATE_WAIT_TO_START Then WaitToStart
    If state.state% = STATE_WAIT_TO_DROP_BLOCK Then WaitToDropBlock
    If state.state% = STATE_WAIT_TO_LAND Then WaitToLand
    UpdateSlice
    UpdateScroll
    state.counter% = state.counter% + 1
End Sub

'======================================================================================================================================================================================================

Sub InitialiseGame
    Dim i%
    game.score& = 0
    game.currentWidth% = BLOCK_START_WIDTH
    i% = 0
    While BLOCK_HEIGHT * i% < FLOOR_HEIGHT
        SetBlockWithWidth block(i%), SCREEN_WIDTH / 2, BLOCK_HEIGHT * (0.5 + i%), SCREEN_WIDTH, 0
        i% = i% + 1
    Wend
    game.blockCount% = i%
    game.gameOver% = FALSE
    slice.active% = FALSE
    game.scroll.active% = FALSE
End Sub

'======================================================================================================================================================================================================

Function mouseOverStartButton%
    Dim mousePos As VECTOR2
    mousePos.x! = (_MouseX - _Width(0) / 2) * SCREEN_WIDTH / _Width(0)
    mousePos.y! = (_Height(0) - _MouseY) * SCREEN_HEIGHT / _Height(0)
    Dim w%, h%
    w% = START_BUTTON_WIDTH
    h% = START_BUTTON_HEIGHT
    mouseOverStartButton% = Abs(mousePos.x!) < w% / 2 And Abs(mousePos.y! - START_BUTTON_Y) < h% / 2
End Function

Sub WaitToStart
    Static mouseUp%, selected%
    If _MouseButton(1) And mouseUp% Then
        selected% = mouseOverStartButton%
        If selected% Then
            PlaySfx SFX_START
            SetGameState STATE_NEW_GAME
            mouseUp% = FALSE
            selected% = FALSE
            Exit Sub
        End If
    End If
    mouseUp% = Not _MouseButton(1)
    If mouseUp% Then selected% = FALSE
End Sub

'======================================================================================================================================================================================================

Sub SetBlockWithWidth (block As BLOCK, x%, y%, w%, v!)
    block.position.x! = x%
    block.position.y! = y%
    block.width% = w%
    block.yVelocity! = v!
    block.positional% = FALSE
End Sub

Sub SetBlockWithLeftRight (block As BLOCK, xLeft%, xRight%, y%, v!)
    Dim width%
    width% = xRight% - xLeft%
    SetBlockWithWidth block, xLeft% + width% / 2, y%, width%, v!
End Sub

Sub ChooseBlockToDrop
    SetBlockWithWidth block(game.blockCount%), BlockX%, BLOCK_START_HEIGHT, game.currentWidth%, 0
    block(game.blockCount%).positional% = TRUE
    game.blockCount% = game.blockCount% + 1
End Sub

Sub WaitToDropBlock
    Static mouseUp%
    block(game.blockCount% - 1).position.x! = BlockX%
    If _MouseButton(1) And mouseUp% Then
        block(game.blockCount% - 1).positional% = FALSE
        block(game.blockCount% - 1).yVelocity! = 0
        SetGameState STATE_WAIT_TO_LAND
        PlaySfx SFX_FALL
        mouseUp% = FALSE
        Exit Sub
    End If
    mouseUp% = Not _MouseButton(1)
End Sub

Function BlockX%
    BlockX% = SCREEN_WIDTH / 2 - (SCREEN_WIDTH - block(game.blockCount% - 1).width%) / 2 * Sin(Timer / 1)
End Function

'======================================================================================================================================================================================================

Sub UpdateScroll
    Dim i%
    If Not game.scroll.active% Then Exit Sub
    game.scroll.count% = game.scroll.count% + 1
    If game.scroll.count% = BLOCK_HEIGHT Then
        game.scroll.active% = FALSE
    End If
    i% = 0
    While i% < game.blockCount%
        If Not block(i%).positional% Then
            block(i%).position.y! = block(i%).position.y! - 1
        End If
        i% = i% + 1
    Wend
    If Not block(0).position.y! < -BLOCK_HEIGHT Then Exit Sub
    i% = 1
    While i% < game.blockCount%
        block(i% - 1) = block(i%)
        i% = i% + 1
    Wend
    game.blockCount% = game.blockCount% - 1
End Sub

Sub UpdateSlice
    If slice.active% Then
        slice.frame% = slice.frame% + 1
        If slice.frame% > SLICE_FADE_FRAMES Then
            slice.active% = FALSE
        Else
            slice.block.position.y! = slice.block.position.y! + slice.block.yVelocity!
            slice.block.yVelocity! = slice.block.yVelocity! + gravity!
        End If
    End If
End Sub

Sub SetSlices (block As BLOCK, target As BLOCK)
    Dim left%, right%
    left% = BlockLeft%(block)
    right% = BlockRight%(block)
    If right% <= BlockLeft%(target) Or left% >= BlockRight%(target) Then
        SetBlockWithLeftRight slice.block, left%, right%, block.position.y!, block.yVelocity!
        slice.active% = TRUE
        slice.frame% = 1
        game.blockCount% = game.blockCount% - 1
        game.gameOver% = TRUE
        Exit Sub
    ElseIf left% < BlockLeft%(target) Then
        SetBlockWithLeftRight slice.block, left%, BlockLeft%(target) - 1, target.position.y! + BLOCK_HEIGHT, 0 'block.yVelocity!
        slice.active% = TRUE
        slice.frame% = 1
        left% = BlockLeft%(target)
    ElseIf right% > BlockRight%(target) Then
        SetBlockWithLeftRight slice.block, BlockRight%(target) + 1, right%, target.position.y! + BLOCK_HEIGHT, 0 'block.yVelocity!
        slice.active% = TRUE
        slice.frame% = 1
        right% = BlockRight%(target)
    End If
    SetBlockWithLeftRight block, left%, right%, target.position.y! + BLOCK_HEIGHT, 0
    game.currentWidth% = block.width%
End Sub

Sub WaitToLand
    WaitForBlockToLand block(game.blockCount% - 1), block(game.blockCount% - 2)
End Sub

Sub WaitForBlockToLand (block As BLOCK, target As BLOCK)
    block.position.y! = block.position.y! + block.yVelocity!
    block.yVelocity! = block.yVelocity! + gravity!
    If block.position.y! <= target.position.y! + BLOCK_HEIGHT Then
        SetSlices block, target
        If Not game.gameOver% Then
            SetGameState STATE_PLAY_TURN
            game.scroll.active% = TRUE
            game.scroll.count% = 0
            game.score& = game.score& + 10
            If game.score& > game.hiscore& Then game.hiscore& = game.score&
            PlaySfx SFX_LAND
        Else
            SetGameState STATE_GAME_OVER
            PlaySfx SFX_LOSE
        End If
    End If
End Sub

Function BlockLeft% (block As BLOCK)
    BlockLeft% = block.position.x! - block.width% / 2
End Function

Function BlockRight% (block As BLOCK)
    BlockRight% = block.position.x! + block.width% / 2
End Function

'======================================================================================================================================================================================================

Function Min% (val1%, val2%)
    If val1% < val2% Then Min% = val1% Else Min% = val2%
End Function

Function Max% (val1%, val2%)
    If val1% > val2% Then Max% = val1% Else Max% = val2%
End Function

'======================================================================================================================================================================================================

Sub RenderBackground
    _glColor4f 1, 1, 1, 0
    _glEnable _GL_TEXTURE_2D
    _glBindTexture _GL_TEXTURE_2D, glData.background&
    _glBegin _GL_QUADS
    _glTexCoord2f 0, 1
    _glVertex2f 0, 360
    _glTexCoord2f 1, 1
    _glVertex2f 480, 360
    _glTexCoord2f 1, 0
    _glVertex2f 480, 0
    _glTexCoord2f 0, 0
    _glVertex2f 0, 0
    _glEnd
    _glDisable _GL_TEXTURE_2D
End Sub

Sub RenderStart
    Dim w%, h%, x%, y%
    w% = START_BUTTON_WIDTH
    h% = START_BUTTON_HEIGHT
    x% = SCREEN_WIDTH / 2
    y% = START_BUTTON_Y
    _glColor4f 1, 1, 1, 1
    _glEnable _GL_TEXTURE_2D
    _glEnable _GL_BLEND
    _glBlendFunc _GL_SRC_ALPHA, _GL_ONE_MINUS_SRC_ALPHA
    If mouseOverStartButton% Then _glColor3f 1, 1, 1 Else _glColor3f 0.5, 0.5, 0.5
    _glBindTexture _GL_TEXTURE_2D, glData.normal&
    _glBegin _GL_QUADS
    _glTexCoord2f 0, 1
    _glVertex2f x% - w% / 2, y% + h% / 2
    _glTexCoord2f 1, 1
    _glVertex2f x% + w% / 2, y% + h% / 2
    _glTexCoord2f 1, 0
    _glVertex2f x% + w% / 2, y% - h% / 2
    _glTexCoord2f 0, 0
    _glVertex2f x% - w% / 2, y% - h% / 2
    _glEnd
    _glDisable _GL_BLEND
    _glDisable _GL_TEXTURE_2D
End Sub

Sub RenderBlocks
    Dim i%
    i% = 0
    While i% < game.blockCount%
        RenderSingleBlock block(i%), 1, 0.75, 0.25, 1
        i% = i% + 1
    Wend
    If slice.active Then
        RenderSingleBlock slice.block, 1, .25, 0.25, 1.0 - slice.frame% / SLICE_FADE_FRAMES
    End If
End Sub

Sub RenderSingleBlock (block As BLOCK, r!, g!, b!, alpha!)
    _glPushMatrix
    _glEnable _GL_BLEND
    _glBlendFunc _GL_SRC_ALPHA, _GL_ONE_MINUS_SRC_ALPHA
    _glTranslatef block.position.x!, block.position.y!, 0
    _glBegin _GL_QUADS
    _glColor4f r!, g!, b!, alpha!
    _glVertex2f -block.width% / 2, BLOCK_HEIGHT / 2
    _glVertex2f block.width% / 2, BLOCK_HEIGHT / 2
    _glVertex2f block.width% / 2, -BLOCK_HEIGHT / 2
    _glVertex2f -block.width% / 2, -BLOCK_HEIGHT / 2
    If block.width% > 4 Then
        _glColor4f r! - 0.25, g! - 0.25, b! - 0.25, alpha!
        _glVertex2f -block.width% / 2 + 2, BLOCK_HEIGHT / 2 - 2
        _glVertex2f block.width% / 2 - 2, BLOCK_HEIGHT / 2 - 2
        _glVertex2f block.width% / 2 - 2, -BLOCK_HEIGHT / 2 + 2
        _glVertex2f -block.width% / 2 + 2, -BLOCK_HEIGHT / 2 + 2
    End If
    _glEnd
    _glPopMatrix
    _glDisable _GL_BLEND
End Sub

Sub RenderNumber (binding&, number&, leftX%, centreY%, charSize%, r!, g!, b!)
    Dim x%, d%, d$, tx!
    d$ = LTrim$(Str$(number&))
    d% = 1
    'x% = centreX% + (d% - 1) * charSize% / 2
    x% = leftX%
    _glColor4f r!, g!, b!, 1
    _glEnable _GL_TEXTURE_2D
    _glEnable _GL_BLEND
    _glBlendFunc _GL_SRC_ALPHA, _GL_ONE_MINUS_SRC_ALPHA
    _glBindTexture _GL_TEXTURE_2D, binding&
    _glBegin _GL_QUADS
    While d% < Len(d$) + 1
        tx! = Val(Mid$(d$, d%, 1)) / 10
        _glTexCoord2f tx!, 1
        _glVertex2f x%, centreY% + charSize% / 2
        _glTexCoord2f tx! + 0.1, 1
        _glVertex2f x% + charSize%, centreY% + charSize% / 2
        _glTexCoord2f tx! + 0.1, 0
        _glVertex2f x% + charSize%, centreY% - charSize% / 2
        _glTexCoord2f tx!, 0
        _glVertex2f x%, centreY% - charSize% / 2
        d% = d% + 1
        x% = x% + charSize%
    Wend
    _glEnd
    _glDisable _GL_BLEND
    _glDisable _GL_TEXTURE_2D
End Sub

'======================================================================================================================================================================================================

Sub RenderFrame
    RenderBackground
    RenderBlocks
    If state.state% = STATE_WAIT_TO_START Then RenderStart
    RenderNumber glData.bubbleText&, game.score&, 102, SCREEN_HEIGHT - 19, 20, 1, 1, 1
    RenderNumber glData.bubbleText&, game.hiscore&, 400, SCREEN_HEIGHT - 19, 20, 1, 1, 1
End Sub

'======================================================================================================================================================================================================

Function LoadTexture& (fileName$)
    LoadTexture& = LoadTextureInternal&(fileName$, FALSE, 0)
End Function

Function LoadTextureWithAlpha& (fileName$, rgb&)
    LoadTextureWithAlpha& = LoadTextureInternal&(fileName$, TRUE, rgb&)
End Function

Function LoadTextureInternal& (fileName$, useRgb%, rgb&)
    Dim img&, img2&, myTex&
    Dim m As _MEM
    img& = _LoadImage(fileName$, 32)
    img2& = _NewImage(_Width(img&), _Height(img&), 32)
    _PutImage (0, _Height(img&))-(_Width(img&), 0), img&, img2&
    If useRgb% Then _SetAlpha 0, rgb&, img2&
    _glGenTextures 1, _Offset(myTex&)
    _glBindTexture _GL_TEXTURE_2D, myTex&
    m = _MemImage(img2&)
    _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGBA, _Width(img&), _Height(img&), 0, _GL_BGRA_EXT, _GL_UNSIGNED_BYTE, m.OFFSET
    _MemFree m
    _FreeImage img&
    _FreeImage img2&
    _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR
    _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
    LoadTextureInternal& = myTex&
End Function

'======================================================================================================================================================================================================

Sub _GL
    If Not glData.executing% Then Exit Sub
    If Not glData.initialised% Then
        glData.initialised% = TRUE
        _glViewport 0, 0, _Width, _Height
        glData.background& = LoadTexture&("assets/background.png")
        glData.normal& = LoadTexture&("assets/start button.png")
        glData.bubbleText& = LoadTexture&("assets/bubble-numbers.png")
    End If
    _glMatrixMode _GL_PROJECTION
    _glLoadIdentity
    _glOrtho 0, _Width, 0, _Height, -5, 5
    _glMatrixMode _GL_MODELVIEW
    _glLoadIdentity
    _glClearColor 0, 0, 0, 1
    _glClear _GL_COLOR_BUFFER_BIT
    RenderFrame
    _glFlush
    If _Exit Then
        exitProgram = TRUE
    End If
End Sub

'======================================================================================================================================================================================================

