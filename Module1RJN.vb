Option Strict On
Imports System.IO
Imports CryptoSysPKI
Imports System.Text



Module Module1RJN
    'Public Declare Sub CopyMemory Lib "Kernel32.dll" Alias "RtlMoveMemory" _
    ' (ByRef dest As Byte, ByRef source As Byte, ByVal numBytes As Integer)
    'Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (ByVal Destination As integer, ByVal Source As integer, ByVal Length As Integer)

    Public currDirect As String = Directory.GetCurrentDirectory, strSessionNumber As String, UUnn As String = "   "
    Public OrigDirectory As String = currDirect & "\", whichlist As String, FileToBeEncrypted() As Boolean, PubKeyDate As DateTime
    Public currDrive As String = Directory.GetDirectoryRoot(currDirect), FlashDriveLetter As String, PubEncrFile As Integer, kjR As Integer
    Public Chunk As Integer = 100000, NumberOfChunks As Integer = 0, LastChunk As Integer = 0, FileLength As Integer, PubDateKey As String, PubDateKeySession As String, qtyAttach As Integer = 1, RecipTo As String = "", SubjeText As String = "", bodConten As String = ""
    Dim numBytesToRead As Integer = Chunk
    Dim numBytesRead As Integer = 0
    Dim nx As Integer
    Public HisName As String = Environment.UserName, TBirdDrive As String, NoTBird As Boolean, StrMod32PadLength As String = "", NewEncryName As String = "", CorrectedFileName As String
    Public PaddedInternalKey As String, Ri(100) As Integer, WeGotOne As Integer, combItem As String, WriterText As String
    Public strSegmentNumber As String, PrivatePassword As String, Cancelled As Boolean, PrePicked As Boolean, LeftPart As String, RightPart As String, strCertName As String = ""
    Public NonceKey As String, PublicKey As String, GettingAutoEncryptPassword As Boolean, FileHolder As String
    Public ByteKey(31) As Byte, AutoEncryptTimerStillDoing As Boolean, AutoEncryptionSequence As Boolean, youremailSEND As String, youremailRECIP As String
    Public RandomTable(32) As Integer, NeedAutoEncrytTimer As Boolean, QuitProcessing As Boolean, RandomViewing As Boolean, NameWasChanged As Boolean
    Public NotModified As Boolean, Cmd As String = "", ComingBack As Boolean, RealName As String
    Public TentNumber As String = Strings.Right(CStr((DateDiff("s", "5/10/1968", Now))), 6), Rate As Integer
    Public Blanking() As Byte = {42, 181, 239, 24, 221, 11, 42, 42, 42}  '"*µïÝ" '
    Public BlankingSTR As String = ChrW(42) & ChrW(181) & ChrW(239) & ChrW(24) & ChrW(221) & ChrW(11) & ChrW(42) & ChrW(42) & ChrW(42) '"*µïÝ***"
    Public BlankingLength As Integer = 12 'Because of ????
    Public TentFile As String, EncryptingFileWithSignature As Boolean, PassOver As Boolean, WrongPassword As Boolean
    Public TentFileDir As String = currDrive & "temp\", ShowingFlashFile As Boolean, DecryptFromPrivateKey As Boolean
    Public TentFileFile As String = "temp" & TentNumber & ".arr", FileNumber As Integer, SessionID As String = ""
    Public InternalKey As String, DiretaSelecta As String, PublicDirectory As String = OrigDirectory & "Public Export\"
    Public GoViewer As Boolean, Folder2ShowinCombo1Box As String, fileSessionID As String
    Public SendersEMail As String, RecipsEMail As String, EncryptingPrivateKey As Boolean, DoingCompID_Registration As Boolean
    Public HeavyPassword As String, CTRNonce As String, wText As String, DoingPublic As Boolean, Looper As Boolean
    Public DarbyCode As Boolean, ChristieNumber As String, Puncher As Boolean, Slug(99) As String
    Public m_lOnBits(30) As Integer, RecipsPublicKey As String, strSessionKey As String, ViewableBytes() As Byte
    Public m_l2Power(30) As Integer, PublicFileNames() As String, HighLightedFile As Integer = 0, doingPublicKey As Boolean
    Public m_bytOnBits(7) As Byte, textConverter As New ASCIIEncoding(), SavedLengthFileName As String
    Public m_byt2Power(7) As Byte, ThisIsFromAnEncryptedFile As Boolean, LastChunkPadding As String = Nothing
    Public strippedPEtemp As String = "stripPE.txt", FileNames() As String, AnalysedFile(,) As String
    Public VirginBytes() As Byte, ViewSequentially As Boolean, EncrypFileName As Boolean, ItsThere As Boolean
    Public m_InCo(3) As Byte, CheckedName As String, pathSource As String, AvaiPhysMemory As Long, RamOK As Boolean, StandardNoteEncrypting As Boolean

    Public m_fbsub(255) As Byte, UseEmailApp As Boolean
    Public m_rbsub(255) As Byte
    Public m_ptab(255) As Byte
    Public m_ltab(255) As Byte
    Public m_ftable(255) As Integer
    Public m_rtable(255) As Integer
    Public m_rco(29) As Integer

    Public m_Nk As Integer
    Public m_Nb As Integer
    Public m_Nr As Integer
    Public m_fi(23) As Byte
    Public m_ri(23) As Byte
    Public m_fkey(119) As Integer
    Public m_rkey(119) As Integer
    'Public Property AllowNull As Boolean
    Public Heavy As String = Chr(109) + Chr(43) + Chr(95) + Chr(34) + Chr(70) + Chr(104) _
 + Chr(115) + Chr(32) + Chr(108) + Chr(96) _
  + Chr(111) + Chr(53) + Chr(69) + Chr(58) + Chr(40) + Chr(37) _
   + Chr(33) + Chr(101) + Chr(90) + Chr(61) _
  + Chr(38) + Chr(48) + Chr(98) + Chr(85) + Chr(120) + Chr(88) _
  + Chr(57) + Chr(35) + Chr(77) + Chr(74) + Chr(56) + Chr(75) _
  + Chr(116) + Chr(82) + Chr(46) + Chr(81) + Chr(76) + Chr(100) _
  + Chr(60) + Chr(79) + Chr(119) + Chr(67) + Chr(39) + Chr(103) _
  + Chr(54) + Chr(68) + Chr(97) + Chr(121) + Chr(52) + Chr(86) _
  + Chr(107) + Chr(110) + Chr(83) + Chr(59) + Chr(91) + Chr(36) _
  + Chr(114) + Chr(117) + Chr(51) + Chr(65) + Chr(118) + Chr(72) _
  + Chr(66) + Chr(62) + Chr(113) + Chr(64) + Chr(44) + Chr(49) _
 + Chr(42) + Chr(55) + Chr(47) + Chr(71) + Chr(93) + Chr(89) _
 + Chr(99) + Chr(78) + Chr(94) + Chr(102) + Chr(122) + Chr(41) _
 + Chr(92) + Chr(50) + Chr(106) + Chr(80) + Chr(73) + Chr(105) _
 + Chr(112) + Chr(87) + Chr(45) + Chr(84) + Chr(63) _
 + Chr(240)
    ReadOnly bytTemp(31) As Byte, ByteCTRNonce() As Byte = Nothing
    'Dim destination As New MemoryStream()
    ' Private bytFileHolder() As Byte
    Dim NNuu As String, ActualLengthFileName As String, PaddedLastChunkwithFileNameLength As Integer
    ReadOnly WorkingLocation As Integer, WorkingChunk As Integer, sFile As String
    Dim OldTentNumber As String



    Public Sub RandShuffle(Ky As String)
        Dim kam As String, Iam As Integer, jam As Integer, ikam As Integer

        On Error GoTo jumpout
        'Len(Heavy= 92
        'Dim HalfPassWd As Integer = CInt(Len(Ky) * 0.5)
        'kam = (Strings.Left(Heavy, HalfPassWd)) + Ky + Strings.Mid(Heavy, Len(Ky) + HalfPassWd, 97 - Len(Ky) - HalfPassWd) ' kam = Strings.Left(Ky + Heavy, 97)
        Dim ShortHeavy As String = Strings.Mid(Heavy, Len(Ky))
        Dim FrontHeavy As String = Strings.Left(Heavy, Len(Ky))
        Dim FH As Integer = Len(FrontHeavy)
        Dim SH As Integer = Len(ShortHeavy)
        kam = Strings.Left(ShortHeavy & Ky & FrontHeavy, 97)
        jam = Len(kam)
        For Iam = 1 To 97
            Ri(Iam) = Strings.Asc(Strings.Mid(kam, Iam, 1)) * 8171717 + Iam * 997
        Next Iam
        kam = ""
        Iam = 97
        jam = 12
        For ikam = 1 To 997
            Ri(Iam) = Ri(Iam) - Ri(jam)
            If Ri(Iam) < 0 Then
                Ri(Iam) = Ri(Iam) + 1000000000
            End If
            If Iam > 1 Then
                Iam = Iam - 1
            Else
                Iam = 97
            End If
            If jam > 1 Then
                jam = jam - 1
            Else
                jam = 97
            End If
        Next ikam
        Ri(98) = 55
        Ri(99) = 24
        Ri(100) = 77
jumpout:
    End Sub
    Sub MakeCTRNonce(ByRef CTRNon As String, ByRef FirstSegment As Boolean)
        Dim intSegmentNumber As Integer

        Dim firstChristie As String = CStr((DateDiff("s", "5/10/1968", Now)) - 1000000000)

        Dim value As Integer = CInt(Strings.Right(firstChristie, 1))
        If value < 2 Then
            value = 9
        End If
        Dim Power As Integer = CInt(Strings.Mid(firstChristie, 7, 1))
        If Power < 2 Then
            Power = 8
        End If
        Dim NewChristie As String = Strings.Right(CStr(CLng(Math.Pow(value, Power))), 9)
        ChristieNumber = Strings.Right(CStr(CLng(firstChristie) + CLng(NewChristie)), 9)



        If FirstSegment = True Then
            intSegmentNumber = 0
        End If
        If Cmd = "DECRYPT" And FirstSegment = True Then
            intSegmentNumber = 1
        End If
        intSegmentNumber += 1
        strSegmentNumber = Strings.Trim(CStr(intSegmentNumber))

        Dim S As String = strSegmentNumber
        S = S.PadLeft(9, "0"c)

        'NonceKey is 14 characters long (9 + 9 + 14 = 32)
        ' Use the IV instead of the password
        NonceKey = Strings.Left(InternalKey, 14)   'since internal key(IV) is always 32 bytes
        CTRNon = ChristieNumber & S & NonceKey
        If Len(CTRNon) < 32 Then
            Stop
        End If
        'Percent = Len(CTRNonce)

    End Sub
    Sub IncrementCTRNonce()
        Dim intSegmentNumber As Integer
        intSegmentNumber = CInt(Strings.Mid(CTRNonce, 10, 9))
        intSegmentNumber += 1
        strSegmentNumber = Strings.Trim(CStr(intSegmentNumber))

        Dim S As String = strSegmentNumber
        S = S.PadLeft(9, "0"c)
        'NonceKey is 14 characters long;    === CTRNonce = (9 + 9 + 14 = 32)
        NonceKey = Strings.Right(CTRNonce, 14)
        CTRNonce = ChristieNumber & S & NonceKey
        If Len(CTRNonce) < 32 Then
            Stop
        End If
    End Sub
    'Sub GetLeftPad(strSegmentNumber)
    '    Dim LenOrig As Integer 'to 9 places
    '    Dim S As String = strSegmentNumber
    '    LenOrig = Len(strSegmentNumber)
    '    If 10 > LenOrig Then
    '        S = S.PadLeft(10 - LenOrig, "0"c)
    '    Else
    '        GetLeftPad = S
    '    End If

    'End Sub
    'Sub PrepareForRijndael(ByteKey() As Byte)
    '    Dim i As Integer
    '    Dim LengInternalKey As Integer = Strings.Len(InternalKey)
    '    Select Case LengInternalKey

    '        Case Is < 14
    '            NonceKey = InternalKey & Strings.Left(Heavy, (14 - LengInternalKey))

    '            PaddedInternalKey = InternalKey & Strings.Left(Heavy, (32 - LengInternalKey))

    '        Case Is = 14
    '            PaddedInternalKey = InternalKey & Strings.Left(Heavy, (32 - LengInternalKey))
    '            NonceKey = InternalKey

    '        Case 15 To 31
    '            NonceKey = Strings.Left(InternalKey, 14)
    '            PaddedInternalKey = InternalKey & Strings.Left(Heavy, (32 - LengInternalKey))

    '        Case Is = 32
    '            PaddedInternalKey = InternalKey
    '            NonceKey = Strings.Left(InternalKey, 14)

    '    End Select

    '    For i = 0 To 31
    '        ByteKey(i) = CByte(Strings.Asc(Strings.Mid(PaddedInternalKey, i + 1, 1)))
    '    Next

    'End Sub
    Sub GenerateInternalKey(word As String)
        'Len(Heavy= 92
        InternalKey = ""
        Do Until Len(InternalKey) >= 32
            InternalKey = InternalKey & word
        Loop
        InternalKey = Strings.Right(InternalKey, 32)
        'Dim ShortHeavy As String = Strings.Mid(Heavy, Len(word))
        'Dim BackHeavy As String = Strings.Left(ShortHeavy, 32 - Len(word))
        ''Dim FH As Integer = Len(FrontHeavy)
        ''Dim SH As Integer = Len(ShortHeavy)
        'HeavyPassword = word & BackHeavy
        ''Create Heavy Password 32 bytes
        'Dim HP As Integer = Len(HeavyPassword)
        'RandShuffle(HeavyPassword)
        ''Dim PIK As Boolean
        ''          Pofevayote1478 = ap%#Ym7kK=awE2A536NgqbRsRJ*3qap%
        ''          pofevayote1478 = pC%#YM7oK=%TEZ@uu43>v>BmRsRJ*sbp
        ''          1478Lefonejist = F*#d$?N%>ykZSj&J*QvKP$eLB=&44GF*
        ''Pofevayote1478Lefonejist = +S5PdiZa2yz@yktCBSaziwsk6+S5PdiZ
        ''pofevayote1478Lefonejist = 6+MPdZq1yWzORykcfCvZgAlBSais66+M
        '' PIK = False
        '' ScrubMemory(InternalKey, PIK)
        'InternalKey = ""

        'Dim i%
        ''Pofevayote1478 >>> InternalKey = ap%#Ym7kK=awE2A536NgqbRsRJ*3qap%
        ''Pofevayote1478Lefonejist                              'InternalKey = "   
        'MakeRandomTable(RandomTable, 32, "GenIntLey")  'based on RandShuffle(HeavyPassword)
        'For i = 1 To 32
        '    InternalKey = InternalKey + Strings.Chr(RandomTable(i))
        '    '        S = S.PadLeft(10 - LenOrig, "0"c) do we do this???
        'Next
        ''For i = 1 To 20     'WHY???? to make table based on 32 charecter password --not necessary
        ''MakeRandomTable(RandomTable, Len(HeavyPassword), "GenIntLey")
        ''Next i

    End Sub
    '---------------------------------------------------------------------------------------
    ' Procedure : ScrubMemory
    ' DateTime  : 3/15/2008 15:03
    ' Author    : Richie L
    ' Purpose   :
    '---------------------------------------------------------------------------------------
    '
    Public Sub ScrubMemory(TagString As String, PIK As Boolean)
        Dim nLen As Integer
        On Error GoTo ScrubMemory_Error

        nLen = Len(TagString)


        If PIK = True Then
            If nLen > 6 Then
                TagString = StrDup(nLen, "6")
                ScrewByteKey(TagString)

                TagString = StrDup(nLen, " ")
                ScrewByteKey(TagString)

                TagString = StrDup(nLen, "*")
                ScrewByteKey(TagString)

                TagString = StrDup(nLen, "^")
                ScrewByteKey(TagString)

                TagString = StrDup(nLen, "R")
                ScrewByteKey(TagString)

                TagString = StrDup(nLen, "L")
                ScrewByteKey(TagString)

                TagString = StrDup(nLen, "B")
                ScrewByteKey(TagString)
            End If
            NotModified = True
            Exit Sub
        End If

        TagString = StrDup(nLen, "6")
        TagString = StrDup(nLen, " ")
        TagString = StrDup(nLen, "*")
        TagString = StrDup(nLen, "^")
        TagString = StrDup(nLen, "R")
        TagString = StrDup(nLen, "L")
        TagString = StrDup(nLen, "B")


        On Error GoTo 0
        Exit Sub

ScrubMemory_Error:

        MsgBox("Error " & Err.Number & " (" & Err.Description & ") in procedure ScrubMemory of Module Module2")
    End Sub

    '---------------------------------------------------------------------------------------
    ' Procedure : ScrewByteKey
    ' DateTime  : 3/15/2008 16:01
    ' Author    : Richie L
    ' Purpose   :
    '---------------------------------------------------------------------------------------
    '
    Private Sub ScrewByteKey(TagString As String)
        ' Dim Rijndee As CRijndael
        'Rijndee = New CRijndael
        For i = 0 To 31
            ByteKey(i) = CByte(Strings.Asc(Strings.Mid(TagString, i + 1, 1)))
        Next
        ' Rijndee.gkey(8, 8, ByteKey)

        On Error GoTo ScrewByteKey_Error



        On Error GoTo 0
        Exit Sub

ScrewByteKey_Error:

        ' MsgBox "Error " & Err.Number & " (" & Err.Description & ") in procedure ScrewByteKey of Module Module2"
    End Sub
    Public Sub MakeRandomTable(RandomTable%(), Howlong As Integer, WhatType As String)
        Dim RTY As Integer, iRIY As Integer, Jriy As Integer, TY As Integer, Rand As Integer, RandInteger%
        On Error GoTo jumpout
        'ReDim RandomTable(Howlong)


again:
        iRIY% = Ri(98)
        Jriy% = Ri(99)
        TY = Ri(iRIY%) - Ri(Jriy%)
        If TY < 0 Then
            TY = TY + 1000000000
        End If
        Ri(iRIY%) = TY
        If iRIY% > 1 Then
            Ri(98) = iRIY% - 1
        Else
            Ri(98) = 55
        End If
        If Jriy% > 1 Then
            Ri(99) = Jriy% - 1
        Else
            Ri(99) = 55
        End If
        iRIY% = Ri(100) Mod 42 + 56
        Ri(100) = Ri(iRIY%)
        Ri(iRIY%) = TY
        Rand = Ri(100)
        Dim RiV As Integer = 1
        Do Until RTY = 33
            RTY += 1
            RandInteger% = CInt((Ri(RiV) Mod 256))
            RiV += 1
            If RiV = 98 Then
                RTY -= 1
                GoTo again
            End If
            RandomTable%(RTY) = RandInteger%
            If WhatType = "GenIntLey" Then
                Select Case RandomTable%(RTY)
                    Case Is < 35, 39 To 41, 44 To 48, 58, 59, 91 To 96, Is > 122
                        RTY = RTY - 1
                End Select
            End If

        Loop
jumpout:
    End Sub

    Public Sub Dismount()
        Dim PIK As Boolean
        On Error Resume Next


        'Kill PublicDirectory$ & "\*.*"
        Clipboard.Clear()
        If InternalKey <> "" Then
            PIK = False
            ScrubMemory(InternalKey$, PIK)
            PIK = False
            ScrubMemory(PrivatePassword$, PIK)
            PIK = False
            ScrubMemory(PublicKey$, PIK)
            PIK = False
            ScrubMemory(NonceKey$, PIK)
            PIK = True
            ScrubMemory(PaddedInternalKey$, PIK)

        End If
        Application.Exit()

    End Sub

    Public Sub SmashPicture(SomeFile$)
        Dim zippr(7) As String, ChunkLocation As Integer = 1

        Dim ExposedName As String = " "
        Dim NameLength As Integer, SlashLocation As Integer = 1, LastSlash As Integer
        Dim FileLocation As String ', Tail As String
        FileLength = CInt(FileLen(SomeFile$))
        NameLength = Len(SomeFile)

        If FileLength = 0 Then
            Kill(SomeFile)
            Exit Sub
        End If
        Dim checkdrive As New DriveInfo(currDrive)
        Do While SlashLocation <> 0
            SlashLocation = InStr(SlashLocation + 1, SomeFile$, "\")
            If SlashLocation <> 0 Then
                LastSlash = SlashLocation
            End If
        Loop
        ExposedName = Right(SomeFile$, NameLength% - LastSlash)
        FileLocation$ = Left(SomeFile, LastSlash)

        'Application.Documents(SomeFile).Close(Word.WdSaveOptions.wdDoNotSaveChanges)
        'If InStr(1, ExposedName, ".arr") = 0 Then
        ' Try
        'File.Open(SomeFile, FileMode.OpenOrCreate, FileAccess.Read, FileShare.Delete)
        ' My.Computer.FileSystem.RenameFile(SomeFile$, "noth" & TentNumber & ".ing")
        'Catch ex As Exception
        'MessageBox.Show(ex.Message, "Renaming File - Probably Belongs to Word -Try Again.", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        'End Try
        'SomeFile$ = FileLocation & "noth" & TentNumber & ".ing"
        'Else
        '    SomeFile$ = FileLocation & ExposedName
        'End If

        'FileClose()
        'FileClose()

        SetUpChunks(FileLength)
        zippr$(1) = StrDup(Chunk, "W")
        zippr$(2) = StrDup(Chunk, "1")
        zippr$(3) = StrDup(Chunk, "#")
        zippr$(4) = StrDup(Chunk, "3")
        zippr$(5) = StrDup(Chunk, "^")
        zippr$(6) = StrDup(Chunk, "R")
        zippr$(7) = StrDup(Chunk, "B")


        For chuk = 1 To NumberOfChunks
            If chuk = NumberOfChunks Then
                Chunk = LastChunk
            End If

tryingagain:
            Try
                'thsfile = SomeFile
                'My.Computer.FileSystem.WriteAllBytes.
                ' FileStream Open(1, SomeFile$, OpenMode.Binary, OpenAccess.ReadWrite, OpenShare.LockReadWrite)
                If ShowingFlashFile Then
                    'File.WriteAllText(SomeFile$, zippr$(1))
                Else
                    'If FileLength > 1000000 Then
                    If checkdrive.IsReady Then
                        If InStr(FileLocation$, "User") > 0 Then
                            SomeFile = System.IO.Path.Combine(My.Computer.FileSystem.SpecialDirectories.MyDocuments, ExposedName)
                        End If
                        For id = 1 To 7
                            My.Computer.FileSystem.WriteAllText(SomeFile, zippr$(id), False)
                        Next
                    Else
                        MessageBox.Show("Your Drive is not Ready. Click OK to try again", "Drive not Ready", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                        FileClose(1)
                        GoTo tryingagain
                    End If
                End If
            Catch ex As Exception

                'MessageBox.Show(ex.Message, "Line 477 or 482", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                If ex.Message = "Access to the path is denied." Then
                    ' MessageBox.Show("This Folder" + combItem + "is a Protected Folder. Go into your Settings, Click on Windows Security, then Click on Open Windows Security, then Click on Virus & threat protection, then Click on Manage
                    '                ransomware protection, then under Controlled folder access, Click Allow an app through Controlled folder access, 
                    'then Click Add an allowed app, then Click Recently blocked apps, then Click the Plus sign next to AwayRJNCryptography.exe, then at the bottom of the page, Click Close.",
                    '                                    ex.Message, MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                End If

                Exit For
            End Try
            FileClose(1)

            ChunkLocation = ChunkLocation + 100000
            '            'Debug.Print chuk
            '            If chuk Mod 10 = 0 Then
            '                'frmAWAY32.Refresh
            '                If frmCoMo Then
            '                    frmCopyMove.Show()
            '                    frmCopyMove.Refresh()
            '                    frmCopyMove.Label5.Caption = chuk
            '                End If
            '                frmAWAY32.Label16.Caption = chuk

            '            End If
            '            If chuk Mod 100 = 0 Then
            '                If frmProcessFiles.WindowState <> vbMinimized Then
            '                    frmbuffer.Show()
            '                    frmbuffer.Refresh()
            '                End If
            '                frmAWAY32.BufferLoop()
            '                frmbuffer.Hide()

            '                'frmAWAY32.Refresh
            '                If frmCoMo Then
            '                    frmCopyMove.Show()
            '                    frmCopyMove.Refresh()
            '                    frmCopyMove.Label5.Caption = chuk
            '                End If
            '            End If
        Next chuk

        '77:
        Try
            My.Computer.FileSystem.RenameFile(SomeFile, TentNumber & ".txt")
            Dim sw As New StreamWriter(FileLocation$ & TentNumber & ".txt")
            sw.WriteLine("??")

            sw.Close()

            Kill(FileLocation$ & TentNumber & ".txt")
            'Kill(combItem & TentNumber & ".txt")
            'ControlScreen.FillTheList()
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Killing File", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End Try

        '        frmAWAY32.Label16.Caption = ""
        '        If frmCoMo Then
        '            frmCopyMove.Label5.Caption = ""
        '        End If
        '        ThisIsFromAnEncryptedFile = 0
    End Sub



    Public Sub DecryptTheRijndael(ByVal pat As String, ByVal wText As String, ByVal N As Integer)
        'Dim EncryptedCTRNonce As String
        Dim BytesEncryptedCTRNonce(31) As Byte, bytEncryptedFile(31) As Byte
        Dim ic As Integer, LengthWorkingChunk As Integer = 0
        Dim bytComboNumber(33) As Byte        'comboNumber As String
        Dim Location As Integer = 0, strE As String, Place As Integer
        Dim PlainTextChunkLocation As Integer = 0
        Dim FirstSegment As Boolean
        Dim ByteCTRNonce(31) As Byte, bytMod32PadLength(1) As Byte
        Dim Rijndee As New CRijndael
        Dim FirstChunk As Integer, bytWholeFileHolder() As Byte = Nothing
        Dim CumulativeChunk As Integer, LengthWOUunn As Integer, ShrunkLength As Integer
        Dim RemainingChunk As Integer, PaddedShrunkLength As Integer
        Dim VirginSpace As Integer, ChrisChunk As Integer, bytFileHolder() As Byte

        Dim InternalKeyBytes() As Byte = System.Text.Encoding.ASCII.GetBytes(InternalKey)
        'Dim PrivatePasswordBytes() As Byte = System.Text.Encoding.ascii.GetBytes(HeavyPassword)
        Rijndee.gentables()
        Rijndee.gkey(8, 8, InternalKeyBytes)   'uses internal key

        SetUpChunks(CInt(FileLen(pat & wText)))  '**********************************LOOP thru chunks **************

        Dim pathSource As String = pat & wText

        'For chuk = 1 To NumberOfChunks
        '    If chuk = NumberOfChunks Then
        '        Chunk = LastChunk
        '    End If

        'Dim BlankTest As String = ""
        'Using fBSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
        '    Dim BB() As Byte = New Byte(Chunk - 1) {}
        '    numBytesToRead = Chunk
        '    numBytesRead = 0
        '    fBSource.Seek(Location, SeekOrigin.Begin)       'Getting Chunk Size FileHolder
        '    While (numBytesToRead > 0)

        '        Dim nx As Integer = fBSource.Read(BB, numBytesRead, numBytesToRead)
        '        If (nx = 0) Then
        '            Exit While
        '        End If
        '        numBytesRead = (numBytesRead + nx)
        '        numBytesToRead = (numBytesToRead - nx)
        '    End While

        '    BlankTest = System.Text.Encoding.UTF8.GetString(BB)

        'End Using

        'Dim strTemp As String = ""

        'If chuk = 1 Then

        '    If CBool(InStr(BlankTest, BlankingSTR)) = True Then
        '        BlankTest = BlankTest.Replace(BlankingSTR, "")
        '    End If
        'End If

        'Using sw2 As New StreamWriter(combItem & "blkTemp.txt", append:=True)

        '    sw2.Write(BlankTest)

        'End Using


        '    Location = Location + Chunk
        'Next chuk
        'Kill(pat & wText)
        'My.Computer.FileSystem.RenameFile(combItem & "blkTemp.txt", wText)

        LengthWOUunn = CInt(FileLen(pat & wText) - 6)
        '     '133,334 bytes after stretch of chr$(1 to 200)TESTED  OK!!!
        '     'START CHUNKING HERE!!! if more then Shrunk length of 100034
        '     '100,000 chunk = 133,334 stretched;
        '     '100,034 chunk = 133,379 STRETCHED!!!!
        '     'ShrunkLength  = (133334) - 1 - (((133334) - 1) \ 4)
        '     'FileHolder = (100000 + (100000 + 2) \ 3)  'stretched length
        '     'First get the EncryptedChristieNumber then start chunking
        '     ' B64 stretch makes 100,000 bytes to 133, 336
        '********************************************************************************************************
        ' n    ' 100,098 = 133464 after B64 stretch        ************* BackDoor Change in FIRST CHUNK *************************

        Chunk = 0
        NumberOfChunks = 1
        RemainingChunk = 0
        LastChunk = 0
        CumulativeChunk = 0
        FirstSegment = True
        FileHolder = ""
        OldTentNumber = TentNumber
recalc:
        TentNumber = Strings.Right(CStr((DateDiff("s", "5/10/1968", Now))), 6)

        If OldTentNumber = TentNumber Then
            GoTo recalc
        Else
            TentFile = pat & "temp" & TentNumber & ".arr"
        End If

        '     'And UUnn$ = ""

        NumberOfChunks = 1
        BlankingLength = CInt(AnalysedFile(N, 4))
        If (LengthWOUunn >= 133380 + BlankingLength) Then
            FirstChunk = 133380 + BlankingLength
            Chunk = 133336
            RemainingChunk = LengthWOUunn - FirstChunk
            If RemainingChunk > 133380 + BlankingLength Then
                Do Until RemainingChunk < Chunk
                    RemainingChunk = RemainingChunk - Chunk
                    NumberOfChunks = NumberOfChunks + 1
                Loop
                CumulativeChunk = FirstChunk + ((NumberOfChunks - 1) * Chunk)
                LastChunk = LengthWOUunn - CumulativeChunk
                If LastChunk = 0 Then
                    LastChunk = Chunk
                    NumberOfChunks = NumberOfChunks - 1
                End If
            Else
                LastChunk = RemainingChunk

            End If

            'First get EncryptedChristieNumber --->
            LengthWorkingChunk = FirstChunk

        Else
            Chunk = LengthWOUunn
            LengthWorkingChunk = Chunk
            LastChunk = Chunk
            NumberOfChunks = NumberOfChunks - 1
        End If


        If LastChunk > 0 Then
            NumberOfChunks = NumberOfChunks + 1
        End If

        'Get #1, 1, FileHolder
        Location = 0
        Dim fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
        Dim FH() As Byte = New Byte(LengthWorkingChunk - BlankingLength - 1) {}      ' took out -1 10/22
        numBytesToRead = LengthWorkingChunk - BlankingLength           ' - 1
        numBytesRead = 0
        'BlankingLength = Len(Blankingstr)' {42, 181, 239, 24, 221, 11, 42, 42, 42}  '"*µïÝ" '
        'Dim infobyte() As Byte = Blanking
        fsSource.Seek(Location + BlankingLength, SeekOrigin.Begin)       'Getting First Chunk   jump over blamking
        While (numBytesToRead > 0)
            Dim nx As Integer = fsSource.Read(FH, numBytesRead, numBytesToRead)
            If (nx = 0) Then
                Exit While
            End If
            numBytesRead = (numBytesRead + nx)
            numBytesToRead = (numBytesToRead - nx)
        End While
        FileHolder = System.Text.UTF8Encoding.UTF8.GetString(FH)

        fsSource.Close()
        'If Len(FileHolder) <> LengthWorkingChunk Then
        '    Stop
        'End If

        ShrunkLength = Len(FileHolder)


        'ReDim bytWholeFileHolder(Len(FileHolder) - 1)

        bytWholeFileHolder = Cnv.FromBase64(FileHolder)     'Shrink


        '        ' CopyMemory(destinat, source(0), lengthbytF)   SAMPLE
        'UserControl copy memory stream

        ' Copy bytes 16-20 from source to index 22 in destination and display the result. 
        'Buffer.BlockCopy(src, 16, dest, 22, 5)
        Buffer.BlockCopy(bytWholeFileHolder, 0, bytComboNumber, 0, 34)
        Buffer.BlockCopy(bytComboNumber, 0, BytesEncryptedCTRNonce, 0, 32)
        Buffer.BlockCopy(bytComboNumber, 32, bytMod32PadLength, 0, 2)
        ' CopyMemory(bytComboNumber(0), bytWholeFileHolder(0), 34)
        'CopyMemory(BytesEncryptedCTRNonce(0), bytComboNumber(0), 32)
        'CopyMemory(bytMod32PadLength(0), bytComboNumber(32), 2)
        'For j = 0 To 31
        'bytRandomPassword(j) = Convert.ToByte(CChar(Strings.Mid(strRandomPassword, j + 1, 1)))
        'Next
        ' StrMod32PadLength = StrConv(bytMod32PadLength, vbUnicode)
        'StrMod32PadLength = Convert.ToString(bytMod32PadLength)
        ChrisChunk = 34



        '     'Decrypt the ChristieNumber
        Rijndee.Decrypt(BytesEncryptedCTRNonce)
        Rijndee.Decrypt(BytesEncryptedCTRNonce)

        Dim strG As String = "" '= Space$(32)
        For ic = 0 To 31
            strG = strG & (Chr(CInt(BytesEncryptedCTRNonce(ic))))
        Next ic
        CTRNonce = strG
        ChristieNumber = Left$(strG, 9)
        StrMod32PadLength = ""
        For ic = 32 To 33
            StrMod32PadLength = StrMod32PadLength & (Chr(CInt(bytComboNumber(ic))))
        Next
        '     'ic = Len(comboNumber)
        CheckOut(CTRNonce)                   ', TestEnd$)
        If WrongPassword = True Then
            ControlScreen.GreenLight()
            InternalKey = ""
            PrivatePassword = ""

            MsgBox("Your Password has produced an incorrectly Deciphered File. Decrypting has been halted. Press OK and the file will be restored to its original Encrypted condition so you can try Decrypting again.", MsgBoxStyle.OkOnly, "Incorrect Password")
            ComingBack = True
            '         TurnOnButtons()
            ControlScreen.FillTheList()
            Exit Sub
        End If
        '     If InStr(1, ChristieNumber$, "Wrong Password") Then
        '         frmAWAY32.picLight_Click()
        '         On Error Resume Next
        '         frmSignal.Hide()
        '         frmBackground.MousePointer = 0
        '         RealName$ = ChristieNumber$
        '         frmImage.MousePointer = 0
        '         msg$ = "Your Password has produced an incorrectly Deciphered File. Decrypting has been halted. Press OK and the file will be restored to its original Encrypted condition so you can try Decrypting again."
        '         Style% = vbOKOnly         '+ vbExclamation                ' Define buttons.
        '         Title$ = "Incorrect Password"
        '         Response = MsgBox(msg$, Style%, Title$)

        '         frmAWAY32.Refresh()
        '         frmAWAY32.MousePointer = 0
        '         TurnOnButtons()
        '         Unload frmProcessFiles
        '         frmAWAY32.FillTheList()
        '         Exit Sub
        '     End If

        '     'Location = 133381
        '     'Then  Start Chunking
        FirstSegment = True
        If ViewableBytes IsNot Nothing Then
            Array.Clear(ViewableBytes, 0, ViewableBytes.Length)
        End If

        For Chuck = 1 To NumberOfChunks

            If Chuck = NumberOfChunks Then
                Chunk = LastChunk
            End If

            If Chuck > 1 Then
                ChrisChunk% = 0
                'FileHolder = Space$(Chunk)
                'ShrunkLength = Chunk
                LengthWorkingChunk = Chunk
                'pathSource = "c:\2atest\bytesFH5.txt"      '*********************************testing bytesFH5
                Dim fsSourceSubseq As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
                'Dim FH As Integer = Len(FileHolder) - 1
                ReDim FH(LengthWorkingChunk - 1)
                numBytesToRead = LengthWorkingChunk     ' - 1
                numBytesRead = 0
                'fsSourceSubseq.Seek(0, SeekOrigin.Begin)       '******************************testing bytesFH5
                fsSourceSubseq.Seek(Location + BlankingLength - 1, SeekOrigin.Begin)       'Getting Subsequent Chunks    
                While (numBytesToRead > 0)
                    Dim nx As Integer = fsSourceSubseq.Read(FH, numBytesRead, numBytesToRead)
                    If (nx = 0) Then
                        Exit While
                    End If
                    numBytesRead = (numBytesRead + nx)
                    numBytesToRead = (numBytesToRead - nx)
                End While
                FileHolder = System.Text.UTF8Encoding.UTF8.GetString(FH)
                fsSourceSubseq.Close()

                'Place = Len(FileHolder)
                'ReDim bytWholeFileHolder(ShrunkLength - 1)

                'bytWholeFileHolder = Cnv.FromBase64(FileHolder)  'Shrink
                bytWholeFileHolder = Convert.FromBase64String(FileHolder) '********************************************* needed
            End If
            'Chuck = 8                           '*******************************testing bytesFH5

            ''If Chuck = NumberOfChunks Then
            'My.Computer.FileSystem.WriteAllBytes("c:\2atest\bytesFH" & Chuck & ".txt", FH, False)                         '133,336
            'My.Computer.FileSystem.WriteAllText("c:\2atest\textFileHolder" & Chuck & ".txt", FileHolder, False)                   '133,339  ????
            ''My.Computer.FileSystem.WriteAllText("c:\2atest\textChunk5.txt", FileHolder, False)                   '133,339  ????
            'My.Computer.FileSystem.WriteAllBytes("c:\2atest\bytWholeFileHolderSHRUNK" & Chuck & ".txt", bytWholeFileHolder, False)     '100,002
            'End If
            Place = 1
            If EncryptingPrivateKey = True Or UUnn$ = "jj*" Or UUnn$ = "jj$" Or UUnn = "dd&" Or UUnn$ = "jP*" Or UUnn$ = "jP$" Or UUnn = "dP$" Or UUnn = "tj*" Or UUnn = "tP*" Or UUnn = "jPk" Or UUnn = "jjk" Or UUnn = "tPk" Or UUnn = "tjk" Or UUnn = "jPL" Or UUnn = "jjL" Then
                ShrunkLength = UBound(bytWholeFileHolder) - ChrisChunk%
                ReDim bytFileHolder(ShrunkLength)   '-1 ????
                'Buffer.BlockCopy(src, 16, dest, 22, 5)
                Buffer.BlockCopy(bytWholeFileHolder, ChrisChunk, bytFileHolder, 0, ShrunkLength + 1)
                'CopyMemory(bytFileHolder(0), bytWholeFileHolder(ChrisChunk), ShrunkLength + 1)
                '        ' CopyMemory(destinat, source(0), lengthbytF)   SAMPLE
            Else
                'ShrunkLength  = UBound(bytWholeFileHolder) - LBound(bytWholeFileHolder) + 1 - ChrisChunk%
                'Try this out
                ReDim bytFileHolder(ShrunkLength - 1)
                For i = 0 To ShrunkLength - 1
                    bytFileHolder(i) = CByte(Asc(Mid$(FileHolder, i + 1, 1)))
                Next
            End If

            strE = ""

            If GoViewer = False And EncryptingPrivateKey = False Then
                'MemST.Close()
                'Membw.Close()
                'frmProcessFiles.Label3.Caption = ""
                'frmProcessFiles.Label3.Caption = "Decrypting ..."
            End If



            'If Chuck = 1 Then
            '    ' PlainTextChunkLocation = 1
            '    Dim WorkingChunk As Integer = 0
            '    Dim WorkingLocation As Integer = 0
            '    'OriginalBarPass = (ShrunkLength / 32)
            '    'UpperBarPass = OriginalBarPass + 18
            '    'LowerBarPass = OriginalBarPass - 18
            'End If
            PaddedShrunkLength = LengthWOUunn
            Dim SLmod As Integer
            Dim bytDEcryptedFile(31) As Byte
            'ReDim Preserve ViewableBytes(CInt(ShrunkLength - Val(SavedLengthFileName) - Val(StrMod32PadLength)))
            SLmod = ShrunkLength + 1
            SLmod = SLmod Mod 32
            If SLmod <> 0 Then
                ControlScreen.GreenLight()
                InternalKey = ""
                PrivatePassword = ""

                MsgBox("Your Password has produced an incorrectly Deciphered File. Decrypting has been halted. Press OK and the file will be restored to its original Encrypted condition so you can try Decrypting again.", MsgBoxStyle.OkOnly, "Incorrect Password")
                ComingBack = True

                ControlScreen.FillTheList()
                Exit Sub
                ShrunkLength = ShrunkLength - SLmod + 1
            End If
            ReDim Preserve VirginBytes(ShrunkLength)
            For JC = 0 To ShrunkLength - 1 Step 32


                'MakeCTRNonce(CTRNonce, ChristieNumber, FirstSegment)
                'FirstSegment = False
                ByteCTRNonce = System.Text.UTF8Encoding.UTF8.GetBytes(CTRNonce)
                'For ic = 0 To 31
                '    ByteCTRNonce(ic) = CTRNonce  'NOW DECRYPTED!!!
                '    ' 'OriginalNonceByte(ic) = ByteCTRNonce(ic)
                'Next

                ''Array.Copy(bytFileHolder, JC, bytTemp, 0, 32)                       Three Slow CONVERSIONS!!!
                ''Rijndee.CopyBytesASP bytTemp, CLng(0), bytFileHolder, CLng(JC), 32
                ''* CopyBytesASP bytIn, 4, bytMessage, 0, lLength
                'Buffer.BlockCopy(src, 16, dest, 22, 5)
                '*********** JAMS HERE WITH E0001.AAA NAMES ***************
                Try
                    Buffer.BlockCopy(bytFileHolder, JC, bytTemp, 0, 32)
                Catch ex As Exception
                    MsgBox("Error " & Err.Number & " (" & Err.Description & ") Picture may be distorted.", MsgBoxStyle.OkOnly)
                End Try

                'CopyMemory(bytTemp(0), bytFileHolder(JC), 32)           '<---- fastest conversion!!!!

                'If JC = ShrunkLength - 32 Then
                '    Dim lastbyte(31) As Byte
                '    Stop
                '    'CopyMemory(lastbyte(0), bytFileHolder(JC), 32)
                'End If
                ''Encrypt the CTRNonce
                Rijndee.Encrypt(ByteCTRNonce)

                IncrementCTRNonce()


                For ic = 0 To 31
                    bytDEcryptedFile(ic) = ByteCTRNonce(ic) Xor bytTemp(ic)
                    'strE = strE & CStr((Chr(CInt(bytEncryptedFile(ic))))) 'Chr(CInt(bytEncryptedFile(ic)))
                    '         WAS     Mid$(strE, Place, 1) = CStr((Chr$(CInt(bytEncryptedFile(ic)))))
                    'Place = Place + 1
                Next ic
                'Buffer.BlockCopy(src, 16, dest, 22, 5)

                Try
                    Buffer.BlockCopy(bytDEcryptedFile, 0, VirginBytes, JC, 32)
                Catch ex As Exception

                End Try

                'CopyMemory(VirginBytes(JC), bytDEcryptedFile(0), 32)
                ''Open OrigDrive$   "aamia\DecryptNonce.txt" For Binary As #4
                ''Put #4, 1, strE
                ''Close #4
                ''last chunk is - Val(StrMod32PadLength)
                ''LastChunkLength including File Name on end
                'sFile = ""
                'sFile = Strings.Left(strE, (ShrunkLength))
                ''i = Len(sFile$)
                ''ShrunkLength = Len(sFile)

                If GoViewer = False And EncryptingPrivateKey = False Then
                    'If WorkingLocation >= LowerBarPass And WorkingLocation <= UpperBarPass Then

                    '    DoEvents()
                    '    If QuitProcessing = True Then
                    '        Close()
                    '        Exit Sub
                    '    End If
                    '    DoTheBar()
                    '    UpperBarPass = UpperBarPass + OriginalBarPass
                    '    LowerBarPass = LowerBarPass + OriginalBarPass

                    'End If
                End If
                'WorkingLocation = WorkingChunk + JC
            Next JC

            '         ''Open OrigDrive$   "aamia\bytwhol.byt" For Binary As #4
            '         ''Put #4, 1, sFile$
            '         ''Close #4
            If Chuck = 1 Then
                Location = 133381
            Else
                Location = Location + 133336
            End If
            'WorkingChunk = WorkingChunk + ShrunkLength
            RealName = ""
            RamOK = False
            If GoViewer = True Then
                'MsgBox(String.Format("AvailablePhysicalMemory: {0} MBytes", System.Math.Round(My.Computer.Info.AvailablePhysicalMemory / (1024 * 1024)), 2).ToString)
                AvaiPhysMemory = CLng(System.Math.Round(My.Computer.Info.AvailablePhysicalMemory, 2))
                If (ShrunkLength * 2) < AvaiPhysMemory Then
                    RamOK = True
                End If
            End If
            If Chuck = NumberOfChunks Then     '********** LAST CHUCK - could be first!!! ***********
                '   Dim NameStringSize As Integer = Val(SavedLengthFileName$) + Val(StrMod32PadLength)
                Dim ltf As Integer = UBound(VirginBytes)
                Dim vslfn As Integer = CInt((Val(SavedLengthFileName$)))
                'My.Computer.FileSystem.WriteAllBytes("c:\2atest\VirginBytes.txt", VirginBytes, False)
                Dim startpoint As Integer = ltf - vslfn + 1
                For iD = 1 To vslfn
                    RealName = RealName & CStr((Chr(CInt(VirginBytes(startpoint)))))
                    startpoint += 1
                Next
                CheckOut(RealName)
                If WrongPassword = True Then
                    ControlScreen.GreenLight()
                    InternalKey = ""
                    PrivatePassword = ""

                    MsgBox("Your Password has produced an incorrectly Deciphered File. Decrypting has been halted. Press OK and the file will be restored to its original Encrypted condition so you can try Decrypting again.", MsgBoxStyle.OkOnly, "Incorrect Password")
                    ComingBack = True

                    ControlScreen.FillTheList()
                    Exit Sub
                End If
                VirginSpace = CInt(ShrunkLength - Val(SavedLengthFileName) - Val(StrMod32PadLength))
                'VirginFile$ = Space$(VirginSpace)
                ReDim Preserve VirginBytes(VirginSpace)
                ''i = Len(VirginFile$)
                'VirginFile = Strings.Left(strE, VirginSpace)
                'If RamOK = False Then
                'This example appends the data array CustomerData to the file CollectedData.
                ' My.Computer.FileSystem.WriteAllBytes("C:\MyDocuments\CustomerData", CustomerData, True)C:\NewAwayRJN\NewAwayRJN\ComputerNode.vb
                My.Computer.FileSystem.WriteAllBytes(TentFile, VirginBytes, True)
                'ReDim Preserve ViewableBytes(PlainTextChunkLocation + VirginSpace)
                'Buffer.BlockCopy(VirginBytes, 0, ViewableBytes, PlainTextChunkLocation, VirginSpace + 1)
                'Else
                '    ' src=source buffer ,srcOffset=The zero-based byte offset into src., dst()The destination buffer.,dstOffset()The zero-based byte offset into dst.,count()The number of bytes to copy.   
                '    'Buffer.BlockCopy(src, 16, dest, 22, 5)
                '    ReDim Preserve ViewableBytes(PlainTextChunkLocation + VirginSpace)
                '    Buffer.BlockCopy(VirginBytes, 0, ViewableBytes, PlainTextChunkLocation, VirginSpace + 1)
                'End If
                'Using sw2 As New StreamWriter(TentFile, append:=True)
                '    sw2.Write(VirginFile)
                'End Using
                'System.IO.File.WriteAllBytes(Path, Data)
                ' System.IO.File.WriteAllBytes(TentFile, VirginFile)

            Else '******************'First, second etc CHUCKS --- Not the last!************
                'If RamOK = False Then
                My.Computer.FileSystem.WriteAllBytes(TentFile, VirginBytes, True)

                'ReDim Preserve ViewableBytes(PlainTextChunkLocation + ShrunkLength)
                'Buffer.BlockCopy(VirginBytes, 0, ViewableBytes, PlainTextChunkLocation, ShrunkLength + 1)
                'Using sw2 As New StreamWriter(TentFile, append:=True)
                '    sw2.Write(strE)
                'End Using
                '    Else
                '    ReDim Preserve ViewableBytes(PlainTextChunkLocation + ShrunkLength)

                '    Buffer.BlockCopy(VirginBytes, 0, ViewableBytes, PlainTextChunkLocation, ShrunkLength + 1)
                '    'Buffer.BlockCopy(src, 16, dest, 22, 5)
                'End If
            End If
            PlainTextChunkLocation = PlainTextChunkLocation + ShrunkLength + 1
            '         ''frmProcessFiles.Label4.Caption = PlainTextChunkLocation 
            '         '' frmProcessFiles.Refresh

        Next Chuck
        BlankingLength = 0
        If RamOK = True Then  '******************************get txt from end and show in Notepad
            Dim Suffix As String = UCase(Path.GetExtension(RealName))
            If Suffix = ".RTF" Or Suffix = ".TXT" Then
                'frmMainWriter.LinkLabel2.Visible = False
                'frmMainWriter.doc.LoadFile(combItem & ThisFile)
                'frmMainWriter.doc.Select(frmMainWriter.doc.Text.Length, 0)
                frmMainWriter.doc.LoadFile(TentFile)

                'frmMainWriter.doc.Text = Strings.Mid(textConverter.GetString(ViewableBytes), 4)
                'Cannot use this cause it doesn't give RTF format********************************
                SmashPicture(TentFile)
                frmMainWriter.doc.Select(frmMainWriter.doc.Text.Length, 0)
                frmMainWriter.TextBox1.Select(frmMainWriter.Text.Length, 0)
                frmMainWriter.LinkLabel2.Visible = False
                frmMainWriter.ShowDialog()
                'If File.Exists(pat & wText) Then
                '    itsthere = True
                'End If
            Else

                'If RealName.EndsWith("TXT") Or RealName.EndsWith("txt") Then
                '    frmMainWriter.doc.Text = textConverter.GetString(ViewableBytes)
                '    frmMainWriter.ShowDialog()
                'Else
                XPBackground.Text = RealName
                kjR = N
                XPBackground.ShowPicture()
            End If
            'End If
            'picturebox.Image = System.Drawing.Bitmap.FromFile(My.Computer.FileSystem.GetName("jumping alien.gif"))
            'XPBackground.PictureBox1.Image = System.Drawing.Bitmap.FromStream(MemST)
        End If
        'Membw.Close()
        'MemST.Close()
    End Sub

    Public Sub SetUpDECRYPTChunks(FileLength As Integer)
        '133,334 bytes after stretch of chr$(1 to 200)TESTED  OK!!!
        '     'START CHUNKING HERE!!! if more then Shrunk length of 100034
        '     '100,000 chunk = 133,334 stretched;
        '     '100,034 chunk = 133,379 STRETCHED!!!!
        '     'ShrunkLength  = (133334) - 1 - (((133334) - 1) \ 4)
        '     'FileHolder = (100000 + (100000 + 2) \ 3)  'stretched length
        '     'First get the EncryptedChristieNumber then start chunking
        '     ' B64 stretch makes 100,000 bytes to 133, 336


        Dim FirstChunk As Integer, RemainingChunk As Integer, CumulativeChunk As Integer
        NumberOfChunks = 0
        If (FileLength >= 133380) And (UUnn$ = "jj*" Or UUnn$ = "jj$" Or UUnn = "dd&" Or UUnn$ = "jP*" Or UUnn$ = "jP$" Or UUnn = "dP$" Or UUnn = "tj*" Or UUnn = "tP*" Or UUnn = "jPk" Or UUnn = "jjk" Or UUnn = "tPk" Or UUnn = "tjk" Or UUnn = "jPL" Or UUnn = "jjL") Then
            FirstChunk = 133380
            Chunk = 133336
            RemainingChunk = FileLength - FirstChunk
            If RemainingChunk > 133380 Then
                Do Until RemainingChunk < Chunk
                    RemainingChunk = RemainingChunk - Chunk
                    NumberOfChunks = NumberOfChunks + 1
                Loop
                CumulativeChunk = FirstChunk + ((NumberOfChunks - 1) * Chunk)
                LastChunk = FileLength - CumulativeChunk
                If LastChunk = 0 Then
                    LastChunk = Chunk
                    NumberOfChunks = NumberOfChunks - 1
                End If
            Else
                LastChunk = RemainingChunk
                NumberOfChunks = NumberOfChunks + 1
            End If

            'First get EncryptedChristieNumber --->
            ' FileHolder = Space$(FirstChunk)

        Else
            Chunk = FileLength
            'FileHolder = Space$(Chunk)
            LastChunk = Chunk
            NumberOfChunks = NumberOfChunks - 1
        End If


        If LastChunk > 0 Then
            NumberOfChunks = NumberOfChunks + 1
        End If
    End Sub
    Public Sub SetUpChunks(FileLength As Integer)
        NumberOfChunks = 0
        If FileLength > 100000 Then
            Chunk = 100000
            NumberOfChunks = CInt(FileLength \ Chunk)
            LastChunk = CInt(FileLength Mod 100000)
            If LastChunk = 0 Then
                LastChunk = CInt(Chunk)
                NumberOfChunks = NumberOfChunks - 1
            End If
        ElseIf FileLength < 100001 Then

            LastChunk = CInt(FileLength)

        End If
        If LastChunk > 0 Then
            NumberOfChunks = NumberOfChunks + 1
        End If

    End Sub
    Public Sub DemoDir()
        Dim directo As String


        If Directory.Exists(OrigDirectory & "Demo") Then
        Else
            directo$ = OrigDirectory & "Demo"
            Directory.CreateDirectory(directo)

            FileCopy(OrigDirectory & "Click View Image now1.jpg", directo$ & "\Click View Image now1.jpg")
        End If

        If Directory.Exists(currDrive & "Program Files (x86)") Then
            TBirdDrive$ = currDrive & "Program Files (x86)\Mozilla Thunderbird"
        Else
            TBirdDrive$ = currDrive & "Program Files\Mozilla Thunderbird"
        End If
        'On Error GoTo NoGotTbird
        UseEmailApp = False
        If Directory.Exists(TBirdDrive$) Then

            NoTBird = False
        Else
            NoTBird = True
            'MsgBox("Since you do not have Moziilla Thunderbird, do you want to manually attach Encrypted files and necessary keys to your current email app?", MsgBoxStyle.YesNo)
            'If MsgBoxResult.Yes = 6 Then
            '    UseEmailApp = True
            '    NoTBird = True
            'Else
            '    NoticeTBird.Show()
            '    If NoticeTBird.DialogResult = DialogResult.OK Then
            '        UseEmailApp = False
            '        NoTBird = False
            '    Else
            '        UseEmailApp = True
            '        NoTBird = True
            '    End If

            'End If

        End If

        'http://msdn.microsoft.com/en-us/library/system.security.permissions.fileiopermission%28v=vs.90%29.aspx
        'Get Permission for adding and deleteing file
        If Directory.Exists(PublicDirectory) Then
            Dim fileEntries As String() = Directory.GetFiles(PublicDirectory)
            ' Process the list of files found in the directory. 
            Dim fileName As String
            For Each fileName In fileEntries
                Kill(fileName)
            Next fileName
        Else
            Directory.CreateDirectory(PublicDirectory$)
        End If
        '********************************************************   CHANGE ALL TO SUB FOLDERS OF "MY DOCUMENTS" ********************************
        If Directory.Exists(OrigDirectory & "Keys") Then
        Else
            Directory.CreateDirectory(OrigDirectory & "Keys")
        End If
        If Directory.Exists(OrigDirectory & "Certs") Then
        Else
            Directory.CreateDirectory(OrigDirectory & "Certs")
        End If
        If Directory.Exists(OrigDirectory & "Encrypted_Inbox") Then
        Else
            Directory.CreateDirectory(OrigDirectory & "Encrypted_Inbox")
        End If
        If Directory.Exists(OrigDirectory & "Encrypted_Sent_Email") Then
        Else
            Directory.CreateDirectory(OrigDirectory & "Encrypted_Sent_Email")
        End If
    End Sub
    Public Function ParseEMail(ByRef BoxText As String) As String
        Dim LessThan As Integer, MoreThan As Integer, Difference As Integer
        LessThan% = InStr(BoxText, "<")
        MoreThan% = InStr(BoxText, ">")
        Trim(BoxText)
        If (LessThan > 0 And MoreThan > 0) Or (LessThan = 0 And MoreThan > 0) Then
            Difference% = MoreThan - 1 - LessThan
        ElseIf LessThan > 0 And MoreThan = 0 Then
            MoreThan = Len(BoxText)
            Difference = MoreThan - LessThan
        End If
        If Difference > 0 Then
            ParseEMail = Mid$(BoxText, LessThan + 1, Difference)
        Else
            ParseEMail = BoxText
        End If
    End Function
    '---------------------------------------------------------------------------------------
    ' Procedure : SHOWPICTURE
    ' DateTime  : 6/20/2012 19:46
    ' Author    : Rich
    ' Purpose   :
    '---------------------------------------------------------------------------------------
    '
    Public Sub SHOWPICTURE()

        Try

            imgPicture2.ShowDialog()

        Catch

            MsgBox("Error " & Err.Number & " (" & Err.Description & ") in procedure SHOWPICTURE of Module Module2")
        End Try
    End Sub
    Public Sub ShowRsaErrorCodes(ByRef code As Integer)
        Dim Explanation(55) As String
        If code = 9999 Then
            code = 52
        End If

        Explanation(1) = "Cannot open input file"
        Explanation(2) = "Cannot create output file"
        Explanation(3) = "File read error"
        Explanation(4) = "File write error"
        Explanation(5) = "Memory error"
        Explanation(6) = "Parameter is missing or wrong"
        Explanation(7) = "Data in the wrong format"
        Explanation(8) = "Data corrupted"
        Explanation(9) = "Unexpected end of file found"
        Explanation(10) = "Unable to convert"
        Explanation(11) = "Value out of range"
        Explanation(12) = "Duplicate data"
        Explanation(13) = "Misc file IO error"
        Explanation(14) = "Unexpected NULL value"
        Explanation(15) = "Decryption error"
        Explanation(16) = "Data invalid"
        Explanation(17) = "Invalid flag"
        Explanation(18) = "Failed to wipe data"
        Explanation(19) = "Algorithm not supported"
        Explanation(20) = "No data to process"
        Explanation(21) = "No match"
        Explanation(22) = "Bad signature"
        Explanation(23) = "Failed a test e.g. known-answer test"
        Explanation(24) = "Rsa key generator failed to find a prime"
        Explanation(26) = "Data not a valid length"
        Explanation(33) = "Invalid key length"
        Explanation(34) = "Invalid block length"
        Explanation(35) = "Invalid mode"
        Explanation(48) = "Invalid key"
        Explanation(49) = "Invalid block"
        Explanation(51) = "Invalid initialisation vector"
        Explanation(52) = "Miscellaneous or unknown error"
        MessageBox.Show("Error      " & Explanation(code) & "    occured." & vbCrLf &
        "Try again or contact BMC Engineering at support@bmc-engineering.com", "Error Generating Public or Private Keys", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

    End Sub
    Public Sub Encrypt(Cmd$, Pat$, ByVal wText As String, N As Integer)


        '        'Fishy = New clsTwofish
        'ActualPath$ = Pat$
        '        frmAWAY32.MaybeBackSlash(ActualPath, BackSlash)

        '        If DoingCompID_Registration = False And AutoEncryptionSequence = 0 And GoViewer% = 0 And EncryptingPrivateKey = False Then
        '            TurnOffButtons()
        '            'frmProcessFiles.Refresh

        '            'frmProcessFiles.Show
        '            'frmProcessFiles.flbFilesinProcess.Refresh
        '            'frmProcessFiles.SetFocus
        '            'frmProcessFiles.cmdCancel.SetFocus
        '            QuitProcessing = False
        '        End If

        '        Number% = 0
        '        UUnn$ = Space$(3)
        '        ActualLengthFileName$ = Space$(3)
        '        SavedLengthFileName$ = Space$(3)
        '        TestEnd$ = Space$(3)
        '        On Error GoTo 0
        ''        'on error goto
        'If PrivatePassword <> "" Then
        '    PrepareForRijndael(ByteKey)                'Must declare intrnal key before this Preparation
        'End If
        '        'If EncryptingFileWithSignature% Or GoViewer Or EncryptingPrivateKey = True Then
        'KL = N%
        '        GoTo start102

        '        'end If
        '        WL = 0

        '        For KL% = 1 To N%
        'start102:
        '            'FileDone% = 0
        '            PassOver = 0
        '            coded% = 0
        '            If DoingCompID_Registration = False Then
        SortFiles(Cmd, Pat, wText, N)
        '            End If
        If AutoEncryptionSequence = False And GoViewer = False And DoingCompID_Registration = False Then
            If EncryptingFileWithSignature = False And EncryptingPrivateKey = False Then
                If FileToBeEncrypted(N) = False Then
                    If DecryptFromPrivateKey = False Then
                        PassOver = True
                    End If
                End If
            End If
        End If
        'AnalysedFile(kj, 1) = SavedLengthFileName
        'AnalysedFile(kj, 2) = UUnn
        'AnalysedFile(kj, 3) = "RegEncrypted"
        UUnn = AnalysedFile(N, 2)
        '            'If AutoEncryptionSequence Then
        '            '  frmAWAY32.EncryptingLabel.Caption = ""
        '            ' frmAWAY32.EncryptingLabel.Caption = "Searching"
        '            ' End If
        'FileName = wText

        If PassOver Then
            ThisIsFromAnEncryptedFile = False
            'coded% = 0
            GoTo Code0
        End If
        '            If AutoEncryptionSequence Then
        '                frmAWAY32.EncryptingLabel.Caption = ""
        '                If Cmd$ = "ENCRYPT" Then
        '                    frmAWAY32.EncryptingLabel.Caption = "Encrypting"
        '                Else
        '                    frmAWAY32.EncryptingLabel.Caption = "Decrypting"
        '                End If
        '            End If
        '            If DoingCompID_Registration = False And GoViewer = 0 And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then
        '                'frmProcessFiles.Refresh

        '            End If
        '            On Error Resume Next
        '            FileLength = FileLen(ActualPath$ & BackSlash & FileName$)
        '            If Cmd$ = "ENCRYPT" Then

        '                Dim driv As Drive, AdequateDriveSpace As Boolean, ThisList$, DrSpFlag$ ', s As integer
        '                driv = FSO.GetDrive(FSO.GetDriveName(Left$(ActualPath$, 2)))

        '                's = driv.FreeSpace
        '                'lengh& = (FileLength * 3)
        '                'lengh& = Val(s)
        '                If ShowingFlashFile = True Then
        '                    ThisList$ = Left$(WhichList$, (InStr(WhichList$, "flashlist.ini") - 1))
        '                Else
        '                    ThisList$ = Left$(WhichList$, (InStr(WhichList$, "away32.ini") - 1))
        '                End If
        '                If driv.FreeSpace > 0 Then             ' if 0 it may be more then max of driv.FreeSpace or really zero


        '                    If (FileLength * 2) > driv.FreeSpace Then
        '                        msg$ = "There is insufficient space on this drive to ENCRYPT this file. "        ' Define message.
        '                        Style% = vbOKOnly
        '                        Title$ = "Insufficient Space"
        '                        Response = MsgBox(msg$, Style%, Title$)
        '                        ComingBack = 1

        '                        DumpTheForm()
        '                        Exit For
        '                    End If
        '                    'set the adequate flag
        '                    AdequateDriveSpace = True

        '            Open ThisList$ & "awayDrive.ini" For Output As #4
        '               Print #4, AdequateDriveSpace
        '            Close #4
        '                Else   'it is zero
        '                    On Error GoTo noflag                'get the register flag
        '            Open ThisList$ & "awayDrive.ini" For Input As #4
        '               Line Input #4, DrSpFlag$
        '            Close #4
        '                    'if flag then                  'if there was a register flag then it went to zero
        '                    msg$ = "There is insufficient space on this drive to ENCRYPT files. "               ' Define message.
        '                    Style% = vbOKOnly
        '                    Title$ = "Insufficient Space"
        '                    Response = MsgBox(msg$, Style%, Title$)
        '                    ComingBack = 1

        '                    DumpTheForm()
        '                    Exit For

        'noflag:             ' if no flag then continue assuming over max
        '                    On Error GoTo 0     'End If
        '                End If
        '            End If
        '      Open ActualPath$ & BackSlash & FileName$ For Binary As #1
        '            'All files will be processed in TentFile$ then moved as required
        'Open TentFile$ For Binary As #2  --!
        'Dim st2 As Stream = File.Open(TentFile, FileMode.Create, FileAccess.Write)
        'Dim bw2 As New BinaryWriter(st2)

        '            'FileLength = LOF(1)
        '            If FileLength < 25 And GoViewer And frmAWAY32.chkSeqAuto.Value Then
        '                frmAWAY32.ViewingTimer.Enabled = False
        '                frmAWAY32.ViewingTimer.Interval = 10
        '                 frmAWAY32.ViewingTimer.Enabled = True
        '                Exit Sub
        '            Else
        '                frmAWAY32.ViewingTimer.Interval = Rate& * 1000
        '            End If
        '            If DoingCompID_Registration = True Or FileLength > 25 Then
        '                If DoingCompID_Registration = False And GoViewer = 0 And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then
        '                    frmProcessFiles.Label1.Caption = Format(FileLength, "###,###,##0") & " Bytes"
        '                    frmProcessFiles.txtFileinProcess.Text = FileName$
        '                End If
        '               Get #1, FileLength - 2, TestEnd$
        '                If TestEnd$ = "##&" Or TestEnd$ = "  &" Then
        '                    UUnn$ = TestEnd$              'ORIGINAL AWAY
        '                  Put #1, FileLength + 1, "012"
        '                    FileLength = LOF(1)
        '                Else
        '                  Get #1, FileLength - 5, UUnn$
        '                End If
        '                'AWAY32

        ' Get #1, FileLength - 2, SavedLengthFileName$

        '                FishFile = False
        '                RijndaelFile = False
        If Cmd$ = "DECRYPT" Then         '*************************************DECRYPT ***************

            '                    NNuu$ = "  &"
            '                    'EncryptedSuffix$ = UUnn$
            '                    Select Case UUnn$
            '                        Case "##&", "%%&"
            '                            InternalKey$ = PrivatePassword$
            '                            coded% = 6
            '                            ShrunkLength& = FileLength - 6
            'IntermLen& = FileLength - 6
            '                            FishFile = False
            '                            RijndaelFile = False
            '                        Case "ss&"
            '                            InternalKey$ = InternalKey$
            '                            coded% = 6
            '                            ShrunkLength& = FileLength - 6
            '                            IntermLen& = FileLength - 6
            '                            FishFile = False
            '                            RijndaelFile = False
            '                        Case "vv&"
            '                            InternalKey$ = InternalKey$
            '                            coded% = 6
            '                            IntermLen& = FileLength - 6
            '                            ShrunkLength& = (IntermLen& - 1 - (IntermLen& - 1) \ 4)
            '                            FishFile = False
            '                            RijndaelFile = False

            '                        Case "ff&" 'Twofished!!
            '                            coded = 6
            '                            InternalKey$ = InternalKey$
            '                            IntermLen& = FileLength - 6
            '                            ShrunkLength& = (IntermLen& - 1 - (IntermLen& - 1) \ 4)
            '                            FishFile = True
            '                            RijndaelFile = False
            '                        Case "rr&" 'Rijndaeled !!!1

            '                            coded = 6
            '                            IntermLen& = FileLength - 6
            '                            FishFile = False
            '                            RijndaelFile = True
            '                        Case "jj*" 'Viewable (GIF, JPG etc) file
            '                            If DarbyCode = False Then
            '                                coded = 6
            '                                IntermLen& = FileLength - 6
            '                                FishFile = False
            '                                RijndaelFile = True
            '                            Else
            '                                coded% = 0
            '                                ThisIsFromAnEncryptedFile = 0
            '                                GoTo Code0
            '                            End If
            '                        Case "jj$", "dd&"          'NON viewable file
            '                            If DarbyCode = False Then
            '                                If GoViewer = 1 Then
            '                                    'HighLightedFile = HighLightedFile - 1
            '                                    If HighLightedFile = 0 Then

            '                                    End If
            '                                    msg$ = "This is not a still picture file and must be Decrypted to be properly handled. "          ' Define message.
            '                                    Style% = vbOKOnly
            '                                    Title$ = "Not a Still Picture File"
            '                                    Response = MsgBox(msg$, Style%, Title$)
            '                                    'WrongType = 0
            '                                    coded% = 0
            '                                    ThisIsFromAnEncryptedFile = 0
            '                                    GoTo Code0
            '                                Else
            '                                    coded = 6
            '                                    IntermLen& = FileLength - 6
            '                                    FishFile = False
            '                                    RijndaelFile = True
            '                                End If
            '                            Else
            '                                coded% = 0
            '                                ThisIsFromAnEncryptedFile = 0
            '                                GoTo Code0
            '                            End If
            '                        Case "jP*" 'Viewable (GIF, JPG etc) file
            '                            If DarbyCode = True Then
            '                                coded = 6
            '                                IntermLen& = FileLength - 6
            '                                FishFile = False
            '                                RijndaelFile = True
            '                            Else
            '                                coded% = 0
            '                                ThisIsFromAnEncryptedFile = 0
            '                                GoTo Code0
            '                            End If
            '                        Case "jP$", "dP&"          'NON viewable file
            '                            If DarbyCode = True Then
            '                                If GoViewer = 1 Then
            '                                    'HighLightedFile = HighLightedFile - 1
            '                                    If HighLightedFile = 0 Then

            '                                    End If
            '                                    msg$ = "This is not a still picture file and must be Decrypted to be properly handled. "          ' Define message.
            '                                    Style% = vbOKOnly
            '                                    Title$ = "Not a Still Picture File"
            '                                    Response = MsgBox(msg$, Style%, Title$)
            '                                    'WrongType = 0
            '                                    coded% = 0
            '                                    ThisIsFromAnEncryptedFile = 0
            '                                    GoTo Code0
            '                                Else
            '                                    coded = 6
            '                                    IntermLen& = FileLength - 6
            '                                    FishFile = False
            '                                    RijndaelFile = True
            '                                End If
            '                            Else
            '                                coded% = 0
            '                                ThisIsFromAnEncryptedFile = 0
            '                                GoTo Code0
            '                            End If
            '                        Case Else

            '                            coded% = 0
            '                            ThisIsFromAnEncryptedFile = 0
            '                            GoTo Code0
            '                    End Select


            '                    If GoViewer = 0 And EncryptingPrivateKey = False Then
            '                        frmProcessFiles.Label3.Caption = "Decrypting ..."
            '                        'frmProcessFiles.Refresh
            '                    ElseIf EncryptingPrivateKey = False Then
            '                        If frmAWAY32.Option1(1).Value Then
            '                            XPBackground.Caption = "Decrypting ..."
            '                        Else
            '                            frmSignal.Show()
            '                            frmSignal.Refresh()
            '                        End If

            '                        'frmSignal.SetFocus
            '                    End If
            '                    If FishFile = True Then
            '                        Fishy.bKey = InternalKey$
            '                     ReDim bytBuffer(1 To ShrunkLength&) As Byte
            '                     ReDim EncryptedFishFile(1 To ShrunkLength&)
            '                        EncrFishFile$ = Space$(IntermLen&)
            '                        sFile = Space$(ShrunkLength&)
            '                        If ShrunkLength& = 0 Then Stop
            '                     Get #1, 1, EncrFishFile$
            '                        On Error GoTo shrinkerror
            '                        Shrink EncrFishFile$
            '                        On Error GoTo 0
            '                        On Error GoTo ConvertFromUnicode
            '                        bytBuffer() = StrConv(EncrFishFile$, vbFromUnicode)
            '                        On Error GoTo 0
            '                        On Error GoTo FishDecrypt

            '                        EncryptedFishFile = Fishy.bDecrypt(bytBuffer())

            '                        On Error GoTo 0
            '                        On Error GoTo ConvertingtoUnicode
            '                        EncrFishFile$ = StrConv(EncryptedFishFile, vbUnicode)
            '                        EncrFishFile$ = Mid$(EncrFishFile$, 2)
            '                        On Error GoTo 0
            '                        On Error GoTo EquatingSfileToEncrfishfile
            '                        sFile = EncrFishFile$
            '                        ShrunkLength& = Len(sFile)
            '                        If ShrunkLength& = 0 Then Stop 'CAN"T RUN IN VB6 __MUST BE COMPILED!
            '                        LastChunkPadding = ""

            '                        On Error GoTo 0
            '                    ElseIf RijndaelFile = True Then

            '                        If GoViewer = 0 And EncryptingPrivateKey = False Then
            '                            frmProcessFiles.Label3.Caption = ""
            '                            frmProcessFiles.Label3.Caption = " Extracting ..."

            '                            'frmProcessFiles.Refresh
            '                        End If
            '                        If GoViewer Then
            '                            'frmAWAY32.ZOrder 1
            '                        End If
            '                        QuitProcessing = False
            DecryptTheRijndael(Pat, wText, N)
            '                        If InStr(1, ChristieNumber$, "Wrong Password") Or QuitProcessing = True Then
            '                            ComingBack = 1
            '                            Exit Sub
            '                        End If
            '                    Else
            '                        If GoViewer = 0 Then
            '                            frmProcessFiles.Label3.Caption = "Initializing"
            '                            'frmProcessFiles.Refresh
            '                        End If
            '                        sFile = Space$(IntermLen&)
            '                     Get #1, 1, sFile
            '                        If ShrunkLength& <> IntermLen& Then
            '                            Shrink sFile
            '                        End If
            '                        KeyMix(ShrunkLength&, InternalKey$, ShuffleKey$)
            '                        RandShuffle ShuffleKey$
            '                        ShuffleKey$ = ""
            '                        MakeRandomTable(RandomTable(), ShrunkLength&, "")
            '                        If GoViewer = 0 Then
            '                            frmProcessFiles.Label3.Caption = "Decrypting ..."
            '                            'frmProcessFiles.Refresh
            '                        End If
            '                        ProcesX(sFile, RandomTable%())



            '                    End If



            ThisIsFromAnEncryptedFile = True
            '                    Close()
            '                    On Error GoTo MakeRealNameString
            If GoViewer = False Then
                Try
                    If FileLen(TentFile) = 0 Then
                        'RealName$ = "Wrong Password"
                        Throw New ArgumentException("Exception Occured")
                    Else
                        'Dim ltf As Integer = FileLen(TentFile)
                        'Dim vslfn As Integer = (Val(SavedLengthFileName$)) - 1
                        'Dim vlcp As Integer = Val(StrMod32PadLength)
                        'RealName$ = Strings.Mid(TentFile, (ltf - vslfn - vlcp), vslfn)
                        'On Error GoTo 0
                        'If RijndaelFile = False Then
                        '                 Open TentFile$ For Binary As #4
                        '                    Put #4, 1, sFile
                        '                 Close #4
                        'End If
                        'On Error GoTo WhileCheckoutRealName
                        'MsgBox "Real Name is" & RealName$
                        'Stop
                        CheckOut(RealName$)
                        If WrongPassword = True Then
                            ComingBack = True
                            Exit Sub
                        End If
                    End If
                Catch
                    ComingBack = True
                    Exit Sub
                End Try
                '                    If InStr(1, RealName$, "Wrong Password") Then
                '                        frmAWAY32.picLight_Click()
                '                        On Error Resume Next
                '                        frmSignal.Hide()
                '                        frmBackground.MousePointer = 0
                '                        frmImage.MousePointer = 0
                '                        msg$ = "Your Password has produced an incorrectly Deciphered File. Decrypting has been halted. Press OK and the file will be restored to its original Encrypted condition so you can try Decrypting again."
                '                        Style% = vbOKOnly             '+ vbExclamation                ' Define buttons.
                '                        Title$ = "Incorrect Password"
                '                        Response = MsgBox(msg$, Style%, Title$)

                '                        'frmAWAY32.Refresh

                '                        frmAWAY32.MousePointer = 0
                '                        TurnOnButtons()
                '                        frmAWAY32.FillTheList()
                '                        Exit Sub
                '                    End If

                Dim miq As Integer = InStr(RealName$, ".")
                '                    Suffix$ = UCase(Mid$(RealName$, (1 + ((InStr(1, Trim$(RealName$), ".")))), (InStr(1, Trim$(RealName$), "."))))

                Dim Suffix As String = UCase(Path.GetExtension(RealName))
                '                    'i = Len(VirginFile)   'the above puts the file back in its virgin condition.
                '                    'frmAWAY32.Label5.Caption = i
                '                    'ActualPathSlash$ = ActualPath$ & BackSlash
                'If GoViewer = 0 Then
                Dim ferr As Integer = 1, ferrput As Boolean = False
                If DoingPublic = False Then

                    If EncryptingPrivateKey = False Then
                        Kill(Pat & wText)
                        Do Until ferrput = True
                            Try
                                My.Computer.FileSystem.RenameFile(TentFile, RealName)
                                'Rename(TentFile, Pat & RealName)
                                ferrput = True
                            Catch
                                If File.Exists(Pat & Strings.Left$(RealName$, miq - 1) & "_COPY" & ferr & "." & Suffix$) Then
                                    ferr += 1
                                Else
                                    My.Computer.FileSystem.RenameFile(TentFile, Strings.Left$(RealName$, miq - 1) & "_COPY" & ferr & "." & Suffix$)
                                    ferrput = True
                                End If
                            End Try
                        Loop
                        ControlScreen.FillTheList()
                    Else
                        Rename(TentFile, OrigDirectory$ & "Keys\" & "DecryptedPrivateKey.DPK")

                        'FileName$ = "DecryptedPrivateKey.DPK"
                        'Kill ActualPathSlash$ & FileName$)
                    End If
                Else
                    If DecryptFromPrivateKey = True Then

                        Kill(Pat & strippedPEtemp)
                        Kill(Pat & FileNames(N))
                        Do Until ferrput = True
                            Try
                                My.Computer.FileSystem.RenameFile(TentFile, RealName)
                                ferrput = True
                            Catch
                                If File.Exists(Pat & Strings.Left$(RealName$, miq - 1) & "_COPY" & ferr & "." & Suffix$) Then
                                    ferr += 1
                                Else
                                    My.Computer.FileSystem.RenameFile(TentFile, Strings.Left$(RealName$, miq - 1) & "_COPY" & ferr & "." & Suffix$)
                                    ferrput = True
                                End If
                            End Try
                        Loop
                        ControlScreen.FillTheList()
                        'ControlScreen.FillTheList()
                    Else
                        Kill(Pat & wText)
                        My.Computer.FileSystem.RenameFile(TentFile, RealName)

                    End If
                End If




                '                            On Error GoTo 0

                '                            On Error GoTo AddOne
                'LocationTwo:
                'f = 2
                'If ferr > 0 Then
                '    My.Computer.FileSystem.RenameFile(ActualPath$ & FileName$, ActualPath$ & Left$(RealName$, miq - 1) & "_COPY" & ferr% & "." & Suffix$)
                'Else
                '    Rename(tentfile, ActualPath$ & RealName$)
                'End If
                'ElseIf FishFile = True Then
                'frmSignal.Hide()
                'VirginFile$ = ""
                'VirginFile$ = Space$(ShrunkLength - Val(SavedLengthFileName$))
                'VirginFile$ = Left$(sFile, ShrunkLength - Val(SavedLengthFileName$))

                '                     Open TentFile$ For Binary As #1
                '                        Put #1, 1, VirginFile$
                '                     Close #1
                'Exit Sub
                'Else
                '    frmSignal.Hide()
                ' Exit Sub
            End If

            '                    End If
        ElseIf Cmd = "ENCRYPT" Then                'ENCRYPT    ********************************ENCRYPT *************************
            '                    If DoingCompID_Registration = False And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then
            '                        frmProcessFiles.Label3.Caption = "Encrypting ..."
            '                    End If

            '                    ChristieNumber = CStr((DateDiff("s", "5/10/1968", Now)) - 1000000000)            '

            '                    Select Case UUnn$
            '                        Case "##&", "%%&", "ss&", "vv&", "dd&", "ff&", "jP*", "jj*", "jP$", "jj$", "rr&"
            '                            'dd& = Digital Signatureized!!!!
            '                            If DoingPublic = 0 Then
            '                                coded% = 0              'Encrypting files
            '                                PassOver = 1
            '                                GoTo Code0
            '                            End If
            '                        Case "  &"
            '                            coded% = 6
            '                            donebefore = 2
            '                        Case Else

            '                            coded% = 6
            '                            SavedLengthFileName$ = ""
            '                            donebefore = 3
            '                    End Select

            If (DoingPublic = True And EncryptingFileWithSignature = True) Then
                NNuu$ = "dd&"

                'RijndaelFile = True

            Else
                Dim Suffix As String = UCase(Path.GetExtension(Pat & wText))

                Select Case Suffix
                    Case ".BMP", ".WMF", ".EMF", ".JPG", ".JPEG", ".GIF", ".HTM", ".JPE", ".PNG"
                        If DarbyCode Then          ' means encrypted with Darb Pic
                            NNuu$ = "jPk"
                        Else
                            NNuu$ = "jjk"             ' k is with blanking
                        End If
                    Case ".RTF"
                        If DarbyCode Then          ' means encrypted with Darb Pic
                            NNuu$ = "tPk"
                        Else
                            NNuu$ = "tjk"
                        End If
                    Case Else
                        If DarbyCode Then
                            NNuu$ = "jPL"             '  L is with Blanking
                        Else
                            NNuu$ = "jjL"
                        End If
                End Select

                'RijndaelFile = True
            End If

            '                    NameDifference% = 0
            '                    ActualLengthFileName$ = NamePad(FileName$)
            '                    If Val(ActualLengthFileName$) < Val(SavedLengthFileName$) Then
            '                        NameDifference% = Val(SavedLengthFileName$) - Val(ActualLengthFileName$)
            '                    End If

            '               On Error GoTo ReadOnly
            '                    Select Case donebefore           'Tacking Names on the End
            '                        Case 2        'Was Encrypted
            '                     Put #1, FileLength - 5 - Val(SavedLengthFileName$) + NameDifference%, wText$(KL%)
            '                            ShrunkLength& = LOF(1) - 6
            '                            'Open ActualPath$ & BackSlash & FileName$ For Binary As #1

            '                        Case 3        'Virgin File

            '                            ShrunkLength& = LOF(1)

            '                            'Put #2, ShrunkLength&, wText$(KL%)

            '                    End Select
            '                    donebefore = 0
            '                    'UUnn$ = Cmd
            TentFile = Pat & "temp" & TentNumber & ".arr"
            DoTheRijndael(Pat, wText)         'comes out as strEncryptedFile (String)

            '                    'TentFileDir$ = OrigDrive$ & "TEMP\"
            '                    'TentFileFile$ = "temp.arr"
            '                    'If QuitProcessing = False Then
            '                    ActualPathSlash$ = ActualPath$ & BackSlash       'REGULAR ENCRYPTION
            '                    'BeginProgressCopy TentFileDir$, TentFileFile$, ActualPathSlash$, "temp.txt"
            '                    'Name ActualPathSlash$ & TentFileFile$ As ActualPathSlash$ & "temp.txt"
            '                    FileCopy(TentFile, ActualPath$ & BackSlash & "temp.txt")

            '                    'Else
            '                    '  ComingBack = 1
            '                    'End If

            '                    Kill TentFile


            '                    On Error GoTo AddOne
            '                    f = 0
            '                    'If QuitProcessing = False Then
            Dim result As String, ferr As Integer, ENumber As Integer = 1, successENumber As Boolean = False

            result = Path.GetFileNameWithoutExtension(Pat & wText)
            If DoingCompID_Registration = True Or EncryptingPrivateKey = True Then
                SmashPicture(Pat & wText)
                My.Computer.FileSystem.RenameFile(TentFile, Pat & result)

            Else
                If ControlScreen.chkEncryptFilenames.Checked And Puncher = False Then


                    Dim ENumberSet As New HashSet(Of Integer)

                    'Type:           System.Boolean()
                    'true if the element is added to the HashSet(Of T) object; false if the element is already present.
                    Do Until successENumber = True

                        If ENumberSet.Add(ENumber) = True Then
                            Try
                                My.Computer.FileSystem.RenameFile(TentFile, "E" & NumPad(ENumber) & ".AAA")
                                ENumber += 1
                                successENumber = True
                                NewEncryName = "E" & NumPad(ENumber) & ".AAA"
                            Catch
                                ENumber += 1
                            End Try
                        Else
                            ENumber += 1
                        End If
                    Loop

                Else
                    'LocationOne:
                    '                            f = 1
                    Do Until successENumber = True
                        Try
                            My.Computer.FileSystem.RenameFile(TentFile, result & ".AAA")
                            successENumber = True
                            NewEncryName = result & ".AAA"
                        Catch ex As FileNotFoundException
                            Stop
                        Catch ex As Exception
                            Dim noote As String = ""
                            noote = ex.Message
                            If File.Exists(Pat & result & "_COPY" & ferr & ".AAA") Then
                                ferr += 1
                            Else
                                My.Computer.FileSystem.RenameFile(TentFile, result & "_COPY" & ferr & ".AAA")
                                successENumber = True
                                NewEncryName = result & "_COPY" & ferr & ".AAA"
                            End If
                        End Try
                    Loop
                    'REGULAR ENCRYPTION

                    If Puncher = False Then
                        'ControlScreen.FillTheList()

                    End If


                End If


            End If
            SmashPicture(Pat & wText)
            'wText = NewEncryName
            '                        frmAWAY32.SmashPicture ActualPathSlash & FileName$
        End If
        '                    If DoingCompID_Registration = False And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then
        '                        'frmProcessFiles.Refresh
        '                    End If
        '                    'End If
        '                End If
        '                If DoingCompID_Registration = False And GoViewer = 0 And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then
        '                    FileDone% = FileDone% + 1
        '                    frmProcessFiles.Label2.Caption = FileDone% & " out of " & FileNumber% & " are done."
        '                    If NeedAutoEncrytTimer And PassOver = 0 Then
        '                        frmAWAY32.FillTheList()
        '                    End If
        'End If
        '                WL = WL + 1
Code0:
        '                Close()

        '                ferr% = 0
        '                f = 0

        '                On Error GoTo 0
        'cont:
        '                If EncryptingFileWithSignature% Then
        '                    WL = KL%
        '                End If

        '                DoEvents()
        '                If QuitProcessing = True Then
        '                    If AutoEncryptTimerStillDoing Then
        '                        Exit For
        '                    Else
        '                        QuitProcessing = False
        '                        ComingBack = 1
        '                        Exit For
        '                    End If
        '                End If
        '            End If
        '            'If ConvertingFromPrivateEncrypttoPublic Then
        '            '  ConvertingFromPrivateEncrypttoPublic = False
        '            ' Cmd$ = "ENCRYPT"

        '            'wText$(KL%) = RealName$
        '            'GoTo start102
        '            'End If
        '            If EncryptingFileWithSignature% Then
        '                Unload frmProcessFiles
        '                ComingBack = 0
        '                GoTo start103
        '            End If
        '        Next KL%


        'start103:


        'AddOne:

        '        ferr% = ferr% + 1
        '        Number = Number + 1

        '        If Number% > 30000 Then
        '            Number = 0
        '            msg$ = "   This file or path  " & ActualPath$ & BackSlash & wText$(KL%) & " is causing an error. You need to cut the number of files in this folder at least in half. "
        '            msg$ = msg$ & vbCrLf & "   Click OK to Continue  "
        '            Style% = vbOK
        '            Title$ = "File Error"
        '            Response = MsgBox(msg$, Style%, Title$)
        '            If Response = vbOK Then
        '                GoTo cont
        '            End If
        '        End If

        '        Select Case f
        '            Case 0
        '                Resume
        '            Case 1
        '                Resume LocationOne
        '            Case 2
        '                Resume LocationTwo
        '        End Select

        'ReadOnly:
        '        On Error GoTo 0
        '        For IW% = 0 To 25
        '            DD$ = Chr$(IW% + 65) & ":\"
        '            If DD$ = UCase(Left$(ActualPath$ & BackSlash, 2)) & "\" Then

        '                Drv = GetDriveType(DD$)
        '                'Public Const DRIVE_REMOVABLE(floppy) = 2
        '                'Public Const DRIVE_FIXED = 3
        '                'Public Const DRIVE_REMOTE = 4
        '                'Public Const DRIVE_CDROM = 5
        '                'Public Const DRIVE_RAMDISK = 6

        '                Select Case Drv

        '                    Case 3, 4
        '                        msg$ = "    " & ActualPath$ & BackSlash & FileName$ & " is Read Only.    "
        '                        msg$ = msg$ & vbCrLf & "   Click OK to change or Cancel to ignore  "
        '                        Style% = vbOKCancel
        '                        Title$ = "Read Only File"
        '                        Response = MsgBox(msg$, Style%, Title$)
        '                        If Response = vbOK Then
        '               Close #1

        '                            SetAttr(ActualPath$ & BackSlash & FileName$, vbNormal)
        '               Open ActualPath$ & BackSlash & FileName$ For Binary As #1
        '                            Resume
        '                        Else
        '                            coded = 0
        '                            PassOver = 1
        '                            GoTo Code0
        '                        End If
        '                    Case 5
        '                        msg$ = "You can't encrypt directly to a CD. Encrypt files on your hard drive and then copy them to your CD-RW."
        '                        Style% = vbOKOnly                ' Define buttons.
        '                        Title$ = "Wrongful Encrypting"
        '                        Response = MsgBox(msg$, Style%, Title$)
        '                        If Response = vbOK Then          ' User chose Yes.

        '                            coded = 0
        '                            PassOver = 1
        '                            GoTo Code0
        '                        End If
        '                End Select
        '            End If
        '        Next
    End Sub
    Sub DoTheRijndael(ByVal pat As String, ByVal wText As String)

        '        On Error GoTo 2130
        '        'If Cmd$ = "ENCRYPT" Then
        Dim Rijndee As New CRijndael
        'Rijndee = New CRijndael
        Dim InternalKeyBytes() As Byte = System.Text.Encoding.ASCII.GetBytes(InternalKey)
        'Dim PrivatePasswordBytes() As Byte = System.Text.Encoding.ascii.GetBytes(HeavyPassword)
        Rijndee.gentables()
        Rijndee.gkey(8, 8, InternalKeyBytes)   'uses internal key
        Dim WorkingChunk As Integer
        Dim WorkingLocation As Integer
        Dim strEncryptedFile As String


        'Dim i As Integer
        '        Dim ic As Integer
        '        Dim Kc As Integer
        '        Dim strE$, testString$, CTRNonce As String
        '        Dim strH As String

        Dim bytCarrier(32) As Byte
        '        Dim bytCarrierLength As integer
        Dim Place As Integer, bytFileNamewithPadding() As Byte '= Nothing

        Chunk = 0
        NumberOfChunks = 0
        LastChunk = 0
        Dim PlainTextChunkLocation As Integer = 0
        'Dim EncryptedChunkLocation As Integer = 1
        FileLength = CInt(FileLen(pat & wText))
        Dim FirstSegment As Boolean = True
        Dim bytCarrierLength As Integer
        Dim ByteCTRNonce() As Byte



        SetUpChunks(FileLength)   '= # of chunks, lastchunk value, chunk of 0 or 100,000

        ActualLengthFileName = CStr(Len(wText))
        Dim PaddedLengthFileName As String = ActualLengthFileName
        PaddedLengthFileName = PaddedLengthFileName.PadLeft(3, "0"c)
        Dim FileLengthwithName As Integer = FileLength + Len(wText)
        Dim Mod32PadLengthInt As Integer = 0
        Dim lengthPadforFileLengthwithName As Integer = 32 - (FileLengthwithName Mod 32)
        Dim PadString As String = ""
        PadString = PadString.PadLeft(lengthPadforFileLengthwithName, "0"c)
        Dim FileNamePaddedString As String = PadString & wText
        Dim Length_of_FileNamePaddedString As String = CStr(Len(FileNamePaddedString))
        PaddedLastChunkwithFileNameLength = LastChunk + Len(FileNamePaddedString)
        LastChunkPadding = Strings.Trim(CStr(lengthPadforFileLengthwithName))
        LastChunkPadding = LastChunkPadding.PadLeft(2, "0"c)
        Dim LastChunkPaddingByte() As Byte = Nothing
        WorkingChunk = 0
        MakeCTRNonce(CTRNonce, FirstSegment)
        '        OriginalBarPass& = (Chunk / 32)
        '        UpperBarPass& = OriginalBarPass& + 18
        '        LowerBarPass& = OriginalBarPass& - 18
        For Chuck = 1 To NumberOfChunks
            If LastChunk > 0 And Chuck = NumberOfChunks Then
                Chunk = LastChunk
            Else
                Chunk = 100000
            End If

            '                If DoingCompID_Registration = False And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then
            '                    frmProcessFiles.Label3.Caption = "Encrypting ..."
            '                    'frmProcessFiles.Refresh
            '                End If


            Dim pathSource As String = pat & wText
            Dim bytFileHolder(Chunk - 1) As Byte

            ' Get #1, PlainTextChunkLocation, bytFileHolder()
            'Dim st As FileStream = File.Open(pathSource, FileMode.Open, FileAccess.Read)
            'Using br As New BinallReader(st)
            '    br.BaseStream.Position = CLng(PlainTextChunkLocation)

            '    bytFileHolder = br.ReadBytes(Chunk)

            'End Using

            'Get #1, 1, FileHolder

            Dim fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
            'Dim FH As Integer = Len(FileHolder) - 1


            numBytesToRead = Chunk
            numBytesRead = 0


            fsSource.Seek(PlainTextChunkLocation, SeekOrigin.Begin)       'Getting First Chunk
            While (numBytesToRead > 0)
                nx = fsSource.Read(bytFileHolder, numBytesRead, numBytesToRead)
                If (nx = 0) Then
                    Exit While
                End If
                numBytesRead = (numBytesRead + nx)
                numBytesToRead = (numBytesToRead - nx)
            End While

            fsSource.Close()

            ''  TEST ************************
            'Dim testHolder As String = Cnv.ToBase64(bytFileHolder)
            'nx = Len(testHolder)
            'Using sw2 As New StreamWriter(pat & "firstwrite.arr")

            '    sw2.Write(testHolder)
            'End Using
            ''Using sw2 As New StreamWriter(TentFile, append:=True)

            ''    sw2.Write(VirginFile)
            ''End Using
            'Dim encryptedString As String = ""
            ''numBytesToRead = nx - 1   'Chunk - 1
            ''numBytesRead = 0
            'Dim sr8 As New StreamReader(pat & "firstwrite.arr")
            'sr8.BaseStream.Seek(PlainTextChunkLocation, SeekOrigin.Begin)
            '' While (numBytesToRead > 0)
            'encryptedString = sr8.ReadLine
            ''If (nx = 0) Then
            ''    Exit While
            ''End If
            ''numBytesRead = (numBytesRead + nx)
            ''numBytesToRead -= 1
            ''End While
            'nx = Len(encryptedString)
            'Dim bytetestHolder() As Byte = Cnv.FromBase64(encryptedString)
            ''System.IO.File.WriteAllBytes(Path, Data)
            'System.IO.File.WriteAllBytes(pat & "secondwrite.arr", bytetestHolder)


            'If bytFileHolder(0) <> bytetestHolder(0) Then
            '    Stop
            'End If
            ''FileHolder = Cnv.ToBase64(ByteCTRNonce)
            ''  bytFileHolder = Cnv.FromBase64(FileHolder)
            ''         END TEST()**********************************


            PlainTextChunkLocation = PlainTextChunkLocation + 100000

            Dim lengthbytF As Integer
            If LastChunk > 0 And Chuck = NumberOfChunks Then
                CheckOut(FileNamePaddedString)
                bytFileNamewithPadding = System.Text.UTF8Encoding.UTF8.GetBytes(FileNamePaddedString)


                lengthbytF = Len(FileNamePaddedString)

                bytCarrierLength = LastChunk + lengthbytF
                Chunk = bytCarrierLength
                ReDim Preserve bytFileHolder(PaddedLastChunkwithFileNameLength - 1)  'took out -1 -- put it back
                'Buffer.BlockCopy(src, 16, dest, 22, 5)
                Buffer.BlockCopy(bytFileNamewithPadding, 0, bytFileHolder, LastChunk, lengthbytF)
                'CopyMemory(bytFileHolder(LastChunk), bytFileNamewithPadding(0), lengthbytF)
                'My.Computer.FileSystem.WriteAllBytes("c:\2atest\lastline.txt", bytFileHolder, False)
            Else
                bytCarrierLength = Chunk
            End If
            'For hg = 1 To 3
            'Dim testline As String = "AaCcEeMmNn"
            'Dim st8 As Stream = File.Open("C:\2atest\D1g Park_8-1-06_006.txt", FileMode.append, FileAccess.Write)
            'Dim sw8 As New StreamWriter(st8)
            'For i = 1 To (9999)
            '    sw8.Write(testline)
            'Next i
            'sw8.Write("1234567890")
            'For i = 1 To (9999)
            '    sw8.Write(testline)
            'Next i
            'sw8.Write("1234567890")
            'For i = 1 To (9999)
            '    sw8.Write(testline)
            'Next i
            'sw8.Write("1234567890")
            'For i = 1 To (9999)
            '    sw8.Write(testline)
            'Next i
            'sw8.Write("1234567890")
            'For i = 1 To (9999)
            '    sw8.Write(testline)
            'Next i
            'sw8.Write("1234567890")
            'For i = 1 To (9999)
            '    sw8.Write(testline)
            'Next i

            'sw8.Write("1234567890")
            'For i = 1 To (1800)
            '    sw8.Write(testline)
            'Next i
            'For i = 1 To (68)
            '    sw8.Write(testline)
            'Next i
            'sw8.Write("1234567890")
            'sw8.Write("QWER")
            'sw8.Close()
            'Next hg
            'test B64 conversion here
            'text to bytes; encrypt; bytes to B64; B64 to bytes ; DEcrypt
            'ByteCTRNonce = System.Text.Encoding.ascii.GetBytes("12345678901234567890123456789012")
            '' ''Encrypt the CTRNonce
            'Rijndee.Encrypt(ByteCTRNonce)   '*********HERE IT IS *********  
            ' TEST OF ENCRYPT/BASE64/DECRYPT

            'FileHolder = Cnv.ToBase64(ByteCTRNonce)

            'bytFileHolder = Cnv.FromBase64(FileHolder)     'Shrink
            'Rijndee.Decrypt(bytFileHolder)            'TES


            ReDim bytCarrier(bytCarrierLength - 1)

            Dim bytFileHolderPLACE As Integer = 0
            '                'Open OrigDrive$ & "aamia\ANonce.txt" For Binary As #4
            '                'Put #4, 1, ByteCTRNonce
            '                'Close #4
            Place = 0


            For JC = 0 To Chunk - 32 Step 32
                'If JC = 99968 Then Stop
                If FirstSegment = True And Chuck = 1 Then
                    FirstSegment = False

                    ReDim bytCarrier(bytCarrierLength + 33)

                    ByteCTRNonce = System.Text.UTF8Encoding.UTF8.GetBytes(CTRNonce)
                    LastChunkPaddingByte = System.Text.UTF8Encoding.UTF8.GetBytes(LastChunkPadding)
                    'Encrypt the CTRNonce
                    Rijndee.Encrypt(ByteCTRNonce)   '*********HERE IT IS *********  
                    Dim byteCTRNonce2(31) As Byte
                    Buffer.BlockCopy(ByteCTRNonce, 0, byteCTRNonce2, 0, 32)
                    Rijndee.Encrypt(byteCTRNonce2)
                    'Place ENCRYPTED ByteCTRNonce at front of file is 32 bytes long Plus two for padlength
                    'Buffer.BlockCopy(src, 16, dest, 22, 5)
                    Buffer.BlockCopy(byteCTRNonce2, 0, bytCarrier, 0, 32)
                    Buffer.BlockCopy(LastChunkPaddingByte, 0, bytCarrier, 32, 2)
                    ' CopyMemory(bytCarrier(0), ByteCTRNonce(0), 32)
                    ' CopyMemory(bytCarrier(32), LastChunkPaddingByte(0), 2)
                    'Rijndee.Decrypt(ByteCTRNonce)
                    '' bytFileNamewithPadding = Convert.ToByte(Strings.Mid(LastChunkPadding, i - 33, 1))
                    'For i = 32 To 33
                    '    'instead of bytCarrier(i) = bytFileNamewithPadding(i - 32)
                    '    bytCarrier(i) = Convert.ToByte(Strings.Mid(LastChunkPadding, i - 31, 1))
                    'Next
                    Place = 34
                Else
                    IncrementCTRNonce()
                    ByteCTRNonce = System.Text.UTF8Encoding.UTF8.GetBytes(CTRNonce)
                    'Encrypt the CTRNonce
                    Rijndee.Encrypt(ByteCTRNonce)   '*********HERE IT IS ********* 
                End If

                'Dim testString As String = Cnv.ToBase64(ByteCTRNonce)
                'Dim lengTestString As Integer = Len(testString)            32 becomes 44
                'then XOR with CTRNonce
                For ic = 0 To 31
                    bytCarrier(Place) = ByteCTRNonce(ic) Xor bytFileHolder(bytFileHolderPLACE)
                    bytFileHolderPLACE += 1
                    Place += 1
                Next ic


                '                    If DoingCompID_Registration = False And GoViewer = 0 And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then
                '                        If WorkingLocation& >= LowerBarPass And WorkingLocation& <= UpperBarPass Then

                '                            DoEvents()
                '                            If QuitProcessing = True Then
                '                                Close()
                '                                Exit Sub
                '                            End If
                '                            DoTheBar()
                '                            UpperBarPass& = UpperBarPass& + OriginalBarPass&
                '                            LowerBarPass& = LowerBarPass& + OriginalBarPass&

                '                        End If
                '                    End If
                WorkingLocation = WorkingChunk + JC
            Next JC


            WorkingChunk = WorkingChunk + Chunk

            strEncryptedFile = ""    'strE

            strEncryptedFile = Cnv.ToBase64(bytCarrier)
            'If Chuck = NumberOfChunks Then
            '    My.Computer.FileSystem.WriteAllText("c:\2atest\encryptedlastline.txt", strEncryptedFile, False)
            'End If

            'Dim testLine As String = Convert.ToBase64String(bytCarrier)
            'Dim LengthTestLine As Integer = Len(testLine)
            'If strEncryptedFile = testLine Then
            '    Stop
            'End If
            ''***TEST ENCRYPT/DECRYPT
            'bytFileHolder = Cnv.FromBase64(strEncryptedFile)
            'Dim leftbytes(31) As Byte
            'For ivd = 0 To 31
            '    leftbytes(ivd) = bytCarrier(ivd)
            'Next

            'Rijndee.Decrypt(leftbytes)
            '' *** END TEST

            Place = Len(strEncryptedFile)
            '                If DoingCompID_Registration = False And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then
            '                    frmProcessFiles.Label3.Caption = "Extracting ..."
            '                    'frmProcessFiles.Refresh
            '                End If
            '                'Open OrigDrive$ & "test2\ANonce.txt" For Binary As #4
            '                'bytCarrier = Space(LOF(4))
            '                'Get #4, 1, bytCarrier
            '                'Close #4
            'Dim st5 As Stream = File.Open("C:\house repairs\temp2.arr", FileMode.Create, FileAccess.Write)
            'Dim sw5 As New BinaryWriter(st5)

            'sw5.Write(bytCarrier)
            'sw5.Close()
            'st5.Close()
            'strEncryptedFile = Cnv.ToBase64(bytCarrier)
            'Dim st6 As Stream = File.Open("C:\house repairs\tempCONVERTED.arr", FileMode.Create, FileAccess.Write)
            'Dim sw6 As New StreamWriter(st6)

            'sw6.Write(strEncryptedFile)
            'sw6.Close()
            'st6.Close()
            If Chuck = 1 Then


                If (DoingPublic = True And EncryptingFileWithSignature = True) Then
                    Dim SendersB64 As String, pad As String, bytBuffer() As Byte = Nothing
                    SendersB64 = SendersEMail$
                    ' Convert String to Byte array.
                    'Dim array() As Byte = System.Text.Encoding.ASCII.GetBytes(value)

                    bytBuffer = System.Text.UTF8Encoding.UTF8.GetBytes(SendersB64)

                    SendersB64 = Cnv.ToBase64(bytBuffer)
                    Dim S As String = Trim(CStr(Len(SendersB64)))
                    pad = S.PadLeft(3, "0"c)

                    'pad = NumPad(Len(SendersB64))            'COUNTS LENGTH OF SENDERSEMAIL$
                    SendersB64 = pad & SendersB64        'rich@away32.com = 20 (b64)

                    '                         9    +      3 + 20                ;4   ;    10        = 46 - SendersEMail$ = 26
                    strEncryptedFile$ = BlankingSTR & SendersB64 & strSessionNumber & SessionID & strEncryptedFile$
                Else
                    strEncryptedFile$ = BlankingSTR & strEncryptedFile
                End If
            End If
            'If Chuck = NumberOfChunks Then
            '    i = Len(strEncryptedFile$)
            'End If

            Dim sFileStr As String, sFile() As Char
            If Chuck = NumberOfChunks Then   'CONVERT TO CHARACTERS
                sFileStr = strEncryptedFile & NNuu$ & PaddedLengthFileName
                sFile = sFileStr.ToCharArray

            Else

                sFile = strEncryptedFile.ToCharArray

            End If

            '            'wText$(KL%) = "temp.txt"

            'sFile = "jahgajhgajhgajhgajhgajhgahgahajhajhgajhgahg"
            ' Put #2, EncryptedChunkLocation&, sFile
            'All files will be processed in TentFile$ then moved as required
            'Open TentFile$ For Binary As #2  --!
            'Dim st As FileStream = File.Open(OrigDirectory & "PunchFile.dll", FileMode.Open, FileAccess.Read)
            '    Dim br As New BinaryReader(st)
            Using sw2 As New StreamWriter(TentFile, append:=True)

                sw2.Write(sFile)
            End Using

            '            EncryptedChunkLocation& = EncryptedChunkLocation& + Len(sFile)
            '            'JC = Len(sFile)
            '            If DoingCompID_Registration = False And EncryptingPrivateKey = False And AutoEncryptionSequence = 0 Then

            '                'frmProcessFiles.Label4.Caption = EncryptedChunkLocation&
            '                If frmProcessFiles.WindowState <> vbMinimized Then
            '                    frmProcessFiles.Label4.Caption = EncryptedChunkLocation&
            '                    frmProcessFiles.Caption = "Processing Files"
            '                Else
            '                    frmProcessFiles.Caption = EncryptedChunkLocation&
            '                    Sleep(60)
            '                    UpdateWindow(frmProcessFiles.hWnd)
            '                End If


            '                'frmprocessfiles.Refresh
            '            End If
        Next Chuck


        '        Close()
        '        Exit Sub
        '2130:
        '        Exit Sub
    End Sub
    '---------------------------------------------------------------------------------------
    ' Procedure : MakePunchFile
    ' DateTime  : 4/28/2012 13:49
    ' Author    : Rich
    ' Purpose   :
    '---------------------------------------------------------------------------------------
    '
    Public Sub MakePunchFile()
        Dim Filestring As String, namear As String, qni As Integer


        If File.Exists(OrigDirectory & "PunchFile.dll") = False Then
            ChristieNumber$ = CStr(DateDiff("s", "5/10/1968", Now))
            PrivatePassword$ = ChristieNumber$
            GenerateInternalKey(PrivatePassword$)
            Filestring = StrDup(100000, "W")
            ' Filestring$ = String(100000, "W")
            Dim sw As New StreamWriter(combItem & "PunchFile.gif")
            sw.WriteLine(Filestring)
            sw.Close()
            'Open OrigDirectory$ & "PunchFile.gif" For Output As #1
            '   Print #1, Filestring
            'Close #1
            Cmd$ = "ENCRYPT"
            'AnalysedFile(kj, 1) = SavedLengthFileName
            'AnalysedFile(kj, 2) = UUnn
            'AnalysedFile(kj, 3) = "RegEncrypted"
            'AnalysedFile(kj, 4) = "Blanking Length"
            ' Pat$ = OrigDirectory$
            ReDim AnalysedFile(1, 4)
            namear = "PunchFile.gif"
            AnalysedFile(1, 1) = CStr(Len(namear))
            AnalysedFile(1, 2) = "WWW"
            AnalysedFile(1, 3) = "PlainText"
            AnalysedFile(1, 4) = "0"
            AutoEncryptionSequence = True
            Puncher = True
            Encrypt(Cmd$, combItem, namear, 1)
            'frmAWAY32.EncryptingLabel.Caption = ""
            'Unload frmProcessFiles
            Rename(combItem & "PunchFile.AAA", OrigDirectory & "PunchFile.dll")
        End If
        AutoEncryptionSequence = False
        Puncher = False
        'On Error GoTo 0
        PrivatePassword$ = ""
        'ReDim Slug(1000, 100)
        'qni = 0
        'For qni = 1 To 100
        '    Slug(qni) = Space(1000)  'then stick in PunchFile     ROTATE SLUGS
        'Next

        qni = 0


        Using st As FileStream = File.Open(OrigDirectory & "PunchFile.dll", FileMode.Open, FileAccess.Read)
            Dim br As New StreamReader(st)

            Dim slugbuffer(1000) As Char
            For qni = 0 To 99

                br.Read(slugbuffer, 0, 1000)
                Slug(qni) = slugbuffer
                'positi = positi + 1000
                ReDim slugbuffer(1000)
            Next
        End Using

        ' Open OrigDirectory$ & "PunchFile.dll" For Binary As #1
        'For qni = 1 To 100
        '    'Get #1, positi, slug$(qni)

        'Next
        ' Close()
        qni = 0


    End Sub
    Function CleanUpFileName(ByVal InputName As String) As String

        For Each c In Path.GetInvalidFileNameChars()
            If InputName.Contains(c) Then
                NameWasChanged = True
                InputName = InputName.Replace(c, " ")
            End If
        Next
        If InputName.Contains(",") Then
            NameWasChanged = True
            InputName = InputName.Replace(",", " ")
        End If

        CleanUpFileName = InputName
    End Function
    Function NumPad$(Number%)
        Dim N As String, nb As Integer
        N = Strings.LTrim(Strings.RTrim(CStr(Number)))
        nb = CInt(Val(N))
        Select Case nb
            Case Is < 10
                N$ = "000" + N$
            Case Is < 100
                N$ = "00" + N$
            Case Is < 1000
                N$ = "0" + N$
        End Select
        NumPad$ = N$
    End Function
    Public Function RsaReadPrivateKey(strEpkFile As String, strPassword As String) As String
        ' Reads the private key from a PKCS-8 EncryptedPrivateKeyInfo file
        ' (as created by Rsa_MakeKeys)
        ' Returns the key as a base64 string or an empty string on error
        'Dim nLen As Integer
        ' Dim lngRet As Long
        ' How long is PrivateKey string?
        'RsaReadPrivateKey = Space(250)
        RsaReadPrivateKey = Rsa.ReadEncPrivateKey(strEpkFile, strPassword).ToString()
        If RsaReadPrivateKey = "" Then
            'Jxy = Sat.LastError()
            Stop
            Exit Function
        End If
        ' Pre-dimension the string to receive data
        'RsaReadPrivateKey = String(nLen, " ")
        ' Read in the Private Key
        ' lngRet = Rsa.ReadEncPrivateKey(RsaReadPrivateKey, nLen, strEpkFile, strPassword, 0)

    End Function
    'Public Function cnvBytesFromB64Str(strB64 As String) As Object
    '    ' Returns a Variant to an array of bytes decoded from a base64 string
    '    Dim abData() As Byte
    '    Dim nDataLen As Long

    '    ' Set default return value that won't cause a run-time error
    '    cnvBytesFromB64Str = abData

    '    nDataLen = Cnv.BytesFromB64Str(0, 0, strB64)
    '    If nDataLen <= 0 Then
    '        Exit Function
    '    End If
    '    ReDim abData(nDataLen - 1)
    '    nDataLen = CNV_BytesFromB64Str(abData(0), nDataLen, strB64)
    '    If nDataLen <= 0 Then
    '        Exit Function
    '    End If
    '    ReDim Preserve abData(nDataLen - 1)
    '    cnvBytesFromB64Str = abData
    'End Function

    '---------------------------------------------------------------------------------------
    ' Procedure : DecryptPublicEncryptedFile
    ' DateTime  : 4/4/2005 12:43
    ' Author    : Richard L
    ' Purpose   : Decrypts the file sent by Sender which
    'was encrypted with the Public Key
    '---------------------------------------------------------------------------------------
    '
    Public Sub DecryptPublicEncryptedFile(pat As String, NameHere As String, kj%)
        Dim strPrivateKey As String, nBlockLen As Integer, abBlock() As Byte, i As Integer
        Dim nBytes As Integer, abResult() As Byte
        Dim strResult As String = ""
        'If kj = 1 Then
        'On Error GoTo DecryptPublicEncryptedFile_Error
        GenerateInternalKey(PrivatePassword$)
        strPrivateKey = RsaReadPrivateKey(OrigDirectory$ & "Keys\" & SendersEMail$ & ".EPK", InternalKey$)
        ' Check its length - it should be 128 bytes (1024 bits) 256 bytes 2048 bits
        If strPrivateKey <> "" Then
            i = Len(strPrivateKey)
            nBlockLen = Rsa.KeyBytes(strPrivateKey)
        Else

            HighLightedFile = 0
            DecryptFromPrivateKey = False
            Cmd$ = ""
            ' DumpTheForm()

        End If
        'Debug.Print("Private key is " & strPrivateKey)

        ' Given the encrypted data in abBlockstrSessionKey
        ' we use the Rsa function to decrypt the input block
        'using the private key

        ' ***************************  COMPARE PUBDATE KEY   HERE
        Dim PublicKeyFile As String = OrigDirectory$ & "Keys\" & SendersEMail$ & ".PUB"

        PubKeyDate = File.GetLastWriteTime(PublicKeyFile)
        PubDateKey = CStr(PubKeyDate)
        PubDateKey = PubDateKey.PadRight(22, "0"c)
        PubDateKey = Cnv.ToBase64(PubDateKey)             '=32 bytes af= ter b64
        If PubDateKeySession <> PubDateKey Then
            MessageBox.Show("It appears that this Session either came from an expired Public Key or from someone else's Public Key or from an imposter. This cannot be Decrypted.", "Wrong Public Key", MessageBoxButtons.OK)
            ComingBack = True
            Exit Sub
        End If
        ReDim abBlock(Len(strSessionKey) - 1)

        abBlock = Cnv.FromBase64(strSessionKey)             'StrConv(strSessionKey, vbFromUnicode)
        abBlock = Rsa.RawPrivate(abBlock, strPrivateKey)
        'Debug.Print "Rsa_RawPrivate returns " & lngRet
        'If lngRet <> 0 Then
        '    'Debug.Print pkiGetLastError()
        '    Exit Sub
        'Else
        '    ' Display our results in hex format
        '    'Debug.Print "DECR= " & cnvHexStrFromBytes(abBlock)
        'End If

        ' Extract the RANDOM PASSWORD from the resulting block
        If abBlock(0) <> 2 Then
            MsgBox("Failed to decrypt correctly!")
            Exit Sub
        End If

        ' So far so good, we should have non-zero padding until we find a 0
        ' before the end
        For i = 1 To nBlockLen - 1
            If abBlock(i) = 0 Then
                ' Found start of real data we want
                Exit For
            End If
        Next

        ' Copy result we want
        nBytes = nBlockLen - i - 1
        ReDim abResult(nBytes - 1)
        For i = i + 1 To nBlockLen - 1
            abResult(i - nBlockLen + nBytes) = abBlock(i)
        Next

        ' Convert to a string
        'Convert the byte array back into a string.
        '  roundtrip = textConverter.GetString(fromEncrypt)
        strResult = textConverter.GetString(abResult)    '

        ' Do something with it
        'Debug.Print strResult
        ' Kill(OrigDirectory$ & "Keys\" & "DecryptedPrivateKey.DPK")
        'End If
        EncryptingPrivateKey = False
        DecryptFromPrivateKey = True
        Cmd = "DECRYPT"
        '     Suffix = Space(3)
        '     'For i = 1 To HighLightedFile
        'Open PublicDirectory$ & "\" & NameHere$ For Binary As #3
        '     'Open Folder2ShowinCombo1Box$ & FileNames$(i) For Binary As #1
        '     FileLength& = LOF(3)
        '     If FileLength& > 100 Then
        '      Get #3, FileLength& - 5, Suffix
        '     End If
        '     Select Case Suffix
        '         Case "jP$", "jP*", "jj$", "jj*", "rr&", "##&", "ff&", "%%&", "ss&", "vv&"
        '             GenerateInternalKey PrivatePassword
        '         Case "dd&"
        GenerateInternalKey(strResult)
        'End Select
        'Quan = 1
        Dim NewName(1) As String
        NewName(1) = NameHere
        FileLength = CInt(FileLen(pat & NewName(1)))
        Encrypt(Cmd, pat, NewName(1), kj)
        If ComingBack = True Then
            Exit Sub
        End If
        '  Next i
        'Unload frmProcessFiles
        'TurnOnButtons

        'On Error GoTo 0
        Exit Sub

DecryptPublicEncryptedFile_Error:

        MsgBox("Error " & Err.Number & " (" & Err.Description & ") in procedure DecryptPublicEncryptedFile of Module Module2")
    End Sub

    Public Sub CheckOut(RealName$)            ', TestEnd$)
        Dim chk As Integer, AscNumber As Integer, LetChar As String   ', PadnLocation As Integer , DotLocation As Integer
        'On Error GoTo 0
        'On Error GoTo ThisIsIt
        'PadnLocation = 0
        WrongPassword = False
        For chk = 1 To Len(RealName$)
            AscNumber = Asc(Strings.Mid(RealName$, chk, 1))
            LetChar = Strings.Mid(RealName$, chk, 1)
            If AscNumber < 32 Or AscNumber > 126 Then
                If Cmd = "ENCRYPT" Then
                    'Replace(TestString,"o", "i")
                    Replace(RealName, LetChar, "-")
                Else
                    WrongPassword = True
                    'CheckedName = "Wrong Password.XXX"
                    Exit Sub
                End If

            End If
        Next
        'DotLocation = InStr(RealName$, ".")
        'PadnLocation = InStr(DotLocation + 3, RealName$, "n")
        'If PadnLocation Then
        '    Select Case TestEnd$
        '        Case "##&", "%%&"
        '            RealName$ = Left$(RealName$, PadnLocation - 1)
        '    End Select
        'End If
        Exit Sub
ThisIsIt:
        'msg$ = "chk = " & chk% & "AscNumber = " & AscNumber & "Len(RealName) = " & Len(RealName$)
        'Style% = vbOKOnly            '+ vbExclamation                ' Define buttons.
        'Title$ = "Incorrect Password"
        'Response = MsgBox(msg$, Style%, Title$)
        'End
    End Sub
    Sub SortFiles(Cmd$, Pat$, ByVal wText As String, N As Integer)


        Dim Suffix As String = UCase(Path.GetExtension(Pat & wText))
        PassOver = False
        Select Case Suffix$
            Case ".EXE", ".BAT", ".DLL", ".INI", ".CPL", ".ISR", _
               ".CUR", ".CVP", ".ICO", ".ISP", ".CRT", ".IDF", ".HLP", _
               ".COM", ".MSN", ".NRG", ".REG", ".SHS", ".SCR", ".HPJ", _
               ".INF", ".SHB", ".MCC", ".TTF", ".WRL", ".ISU", ".RBP", _
               ".WRZ", ".SYS", ".VXD", ".386", ".DRV", ".MWP", ".IWZ", _
               ".TMP", ".STY", ".DIC", ".UDC", ".D32", ".CNT", ".TAR", _
               ".PIF", ".CAB", ".OCX", ".VBD", ".FRM", ".DDF", ".HHK", _
               ".DEP", ".SWT", ".FRX", ".VBP", ".VBW", ".CHM", ".HHC", _
               ".MDP", ".LNK", ".CER", ".BIN", ".MANIFEST", ".MIF", ".PRX", _
               ".CMD", ".UCE", ".TSP", ".TLB", ".THA", ".TBL", ".SRG", _
               ".SEP", ".SDI", ".RSP", ".RS", ".RAT", ".OLB", ".NLS", _
               ".MOF", ".MSC", ".IME", ".DEP", ".CPX", ".OCA", ".H", ".AX", _
               ".RLL", ".ACM"
            Case ".AAA", ".EPV"
                If Cmd$ = "ENCRYPT" And DoingPublic = False And EncryptingPrivateKey = False Then
                    PassOver = True
                Else
                    PassOver = False
                End If
            Case ""
                PassOver = True
            Case Else
                If Cmd$ = "DECRYPT" And DecryptFromPrivateKey = False Then
                    PassOver = True
                Else
                    PassOver = False
                End If
        End Select
    End Sub
End Module
