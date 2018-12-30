Imports System.IO

Imports Microsoft.VisualBasic
Imports Microsoft.Win32
Imports System.Text
Imports CryptoSysPKI

Imports System.Security.Permissions

Public Class ControlScreen

    Dim Boxing As Boolean
    Dim lBytesTotal, lFreeBytes, lFreeBytesAvailable As Long, SendCert As Boolean, strCertName As String = ""
    Dim ThisFile As String, ThisFileFull As String, intSessionNumber As Integer
    Dim Tail As String, strRandomPassword As String, GotAPublicKey As Boolean
    Public FlashDrive As Boolean, FlashLocation As String, FoundFlashAlready As Boolean
    Public mnuFlashDriveStatus As String
    Private imageList1 As ImageList
    Dim PublicEncrypted As Boolean, startingload As Boolean, SenderPubKeyExists As String

    Dim SmallBreak As String = vbCrLf
    Dim Paragraph As String = vbCrLf & vbCrLf, PublicDirectoryFileNmae As String, PreviousPublicKeyExpired As String
    Dim FileStatus As String, PreviousSessionNumber As String, GetNewSessionInfo As Boolean
    Dim numBytesToRead As Integer = 0, LenmaiL As String

    Dim numBytesRead As Integer = 0
    Dim kj As Integer
    Dim nBlockLen As Integer
    Dim nBytes As Integer
    Dim nPad As Integer
    'Dim sPublicKeyFile As String
    Dim sPublicKey64 As String
    Dim abBlock() As Byte
    Dim colRemovedTabs As New Collection()
    'Dim TabPage10 As TabPage = New TabControl.TabPageCollection()
    Dim screen__1 As Screen = Screen.FromControl(Me)
    Dim workingArea As Rectangle = screen__1.WorkingArea

    Dim i As Integer

    Dim cunt As Integer = 1, Gotcha As Integer = 0



    Public Sub New()

        ' This call is required by the designer.
        Try

            startingload = True
            InitializeComponent()

        Catch ex As Exception
            Dim eventLog As EventLog = New EventLog
            Dim SourceName As String = "WindowsService.ExceptionLog"
            If Not eventLog.SourceExists(SourceName) Then
                eventLog.CreateEventSource(SourceName, "AWAYRJN")
            End If

            eventLog.Source = SourceName
            Dim message As String = String.Format("Exception: {0} " & "currdrive = " & currDrive & "*" & vbLf & vbLf & vbCrLf & "Stack: {1}", ex.Message, ex.StackTrace)
            eventLog.WriteEntry(message, EventLogEntryType.Error)
        End Try



    End Sub


    Private Sub ControlScreen_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Application.EnableVisualStyles()
        Try


            Dim fp As New FileIOPermission(PermissionState.None)
            fp.AllLocalFiles = FileIOPermissionAccess.AllAccess
            Try
                fp.Demand()
            Catch ex As Security.SecurityException
                Console.WriteLine("You have no permission for local files")
                End
            Catch ex As Exception

            End Try
            'Netsh firewall add allowedprogram C:    \MyApp\MyApp.exe MyApp ENABLE

            Me.Visible = False

            Dim CSFlash As New Splash3
            'startingload = True

            CSFlash.Show()
            CSFlash.Refresh()
            ViewSequentially = CBool(GetSetting("winexpl", "startup", "chkSequence", CStr(AutoSequ.CheckState)))
            If ViewSequentially = True Then
                AutoSequ.CheckState = CheckState.Checked
            End If
            EncrypFileName = CBool(GetSetting("winexpl", "startup", "chkEncryptFilenames", CStr(EncrypFileName)))
            If EncrypFileName = True Then
                chkEncryptFilenames.CheckState = CheckState.Checked
            End If
            Looper = CBool(GetSetting("winexpl", "startup", "chkRepeat", CStr(RepeatSequence.CheckState)))
            If Looper = True Then
                RepeatSequence.CheckState = CheckState.Checked
            End If
            RandomViewing = CBool(GetSetting("winexpl", "startup", "chkRandomView", Str(RandomView.Checked)))
            If RandomViewing = True Then
                RandomView.CheckState = CheckState.Checked
            End If
            youremailRECIP = GetSetting("winexpl", "startup", "youremailRECIP", youremailRECIP)
            youremailSEND = GetSetting("winexpl", "startup", "youremailSEND", youremailSEND)
            Rate = CInt(GetSetting("winexpl", "startup", "txtRate", CStr(Rate)))
            If Rate = 0 Then
                Rate = 1
            End If
            NumericUpDown1.Value = Rate
            Dim start, finish As Double
            start = Microsoft.VisualBasic.DateAndTime.Timer
            ' Set end time for 5-second duration.
            finish = start + 4.0
            Do While Microsoft.VisualBasic.DateAndTime.Timer < finish
                ' Do other processing while waiting for 5 seconds to elapse. 
            Loop

            CSFlash.Close()



            'If CDbl(workingArea.Width) > CDbl(1.7 * workingArea.Height) Then
            '    Me.Height = workingArea.Height - 250

            '    '    Me.Width = 1020
            '    '    Me.Height = 520
            '    '    Me.TabControl1.Location = New Point(500, 40)
            '    '    Me.TabControl1.Anchor = AnchorStyles.Top
            '    '    Me.PictureBox2.Location = New Point(16, 437)
            '    '    Me.Label13.Location = New Point(40, 418)
            '    '    Me.Label12.Location = New Point(26, 437)
            '    '    'Me.Button10.Location = New Point(302, 418)
            '    '    Me.ListView1.Size = New Size(461, 300)
            '    '    Me.StartPosition = FormStartPosition.Manual
            '    '    Me.ListView1.Location = New Point(10, 95)

            '    '    Me.Location = New Point() With { _
            '    '     .X = CInt(Math.Max(workingArea.X, workingArea.X + (workingArea.Width - Me.Width) / 2)), _
            '    '     .Y = CInt(Math.Max(workingArea.Y, workingArea.Y + (workingArea.Height - Me.Height) / 2)) _
            '    '    }
            'Else
            '    Me.Height = workingArea.Height - 350

            'End If
            'Dim orangepic As New newleadin
            'orangepic.Show()
            'orangepic.Refresh()


            Me.Location = New Point() With { _
             .X = CInt(Math.Max(workingArea.X, workingArea.X + (workingArea.Width - Me.Width) / 2)), _
             .Y = CInt(Math.Max(workingArea.Y, workingArea.Y + (workingArea.Height - Me.Height) / 2)) _
            }
            Me.chkAutoEncrypt.Visible = False
            Me.Label11.Visible = False
            Me.DoneButton14.Visible = False
            Me.Show()
            Me.Refresh()
            DemoDir()

            'TabControl1.TabPages.RemoveAt(4)    ' ******* Finally from bottom of page ***
            ''http://tinyurl.com/lf9hxtv         
            DoTheBox()
            MakePunchFile()
            KeyPreview = True
        Catch ex As Exception
            Dim eventLog As EventLog = New EventLog
            Dim SourceName As String = "WindowsService.ExceptionLog"
            If Not eventLog.SourceExists(SourceName) Then
                eventLog.CreateEventSource(SourceName, "AWAYRJN")
            End If

            eventLog.Source = SourceName
            Dim message As String = String.Format("Exception: {0} " & vbLf & vbLf & "Stack: {1}", ex.Message, ex.StackTrace)
            eventLog.WriteEntry(message, EventLogEntryType.Error)
            MessageBox.Show("An internal error has occured. Don't close this box yet. Go to Control Panel -> Adnistrative Tools -> Event Viewer -> Application and Services Log -> AWAYRJN. Copy and paste the General Exception into an email to support@bmc-engineering.com. We will then take steps to recreate the error and correct it. Thanks for your patience. :)", "Internal Error", MessageBoxButtons.OK)
        End Try
        startingload = False
    End Sub

    'Sub SetFSW()
    '    fsw.Path = combItem
    '    fsw.IncludeSubdirectories = False
    '    fsw.SynchronizingObject = Me
    '    fsw.EnableRaisingEvents = True
    'End Sub
    Public Sub DoTheBox()
        Boxing = True

        If ShowingFlashFile = True Then
            whichlist = FlashLocation$
        Else
            whichlist = OrigDirectory & "away32.ini"
        End If

        Try

            ComboBox1.BeginUpdate()

            If Me.ComboBox1.SelectedItem IsNot Nothing Then
                ComboBox1.Items.Clear()
            End If
            Try
                Dim sr As StreamReader = New StreamReader(whichlist)

                Do While sr.Peek() >= 0
                    ComboBox1.Items.Add(sr.ReadLine())
                Loop
                sr.Close()

            Catch
                If ShowingFlashFile = False Then
                    Dim sw As New StreamWriter(whichlist)
                    sw.WriteLine(OrigDirectory & "Demo\")
                    If Directory.Exists(currDrive & "Users") Then
                        sw.WriteLine(currDrive & "Users\" & HisName & "\Documents\")
                    Else
                        sw.WriteLine(currDrive & "Documents and Settings\" & HisName & "\My Documents\")
                    End If
                    sw.WriteLine(OrigDirectory & "Encrypted_Inbox\")
                    sw.WriteLine(OrigDirectory & "Encrypted_Sent_Email\")
                    sw.Close()
                Else
                    Dim sw As New StreamWriter(whichlist)
                    sw.WriteLine("None Yet")
                    sw.Close()
                End If
                Dim sr As StreamReader = New StreamReader(whichlist)

                Do While sr.Peek() >= 0
                    ComboBox1.Items.Add(sr.ReadLine())

                Loop
                sr.Close()

            End Try

            If ComboBox1.SelectedIndex = -1 Then
                If Folder2ShowinCombo1Box$ = "" Then
                    ComboBox1.SelectedItem = ComboBox1.Items(0)
                Else
                    ComboBox1.SelectedItem = Folder2ShowinCombo1Box
                End If
            End If

            If ShowingFlashFile = False Then
                combItem = CStr(ComboBox1.SelectedItem)
            Else
                combItem = FlashDriveLetter & CStr(ComboBox1.SelectedItem)
            End If
            ComboBox1.EndUpdate()
            ToolTip1.SetToolTip(ComboBox1, "Click Down Arrow to Select other Special Folders.")
            Boxing = False

            FillTheList()
        Catch ex As Exception

            'MessageBox.Show(ex.Message, "Line 477 or 482", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
            If ex.Message = "Access to the path is denied." Then
                '  MessageBox.Show("This Folder" + combItem + "is a Protected Folder. Go into your Settings, Click on Windows Security, then Click on Open Windows Security, then Click on Virus & threat protection, then Click on Manage
                '                     ransomware protection, then under Controlled folder access, Click Allow an app through Controlled folder access, 
                'then Click Add an allowed app, then Click Recently blocked apps, then Click the Plus sign next to AwayRJNCryptography.exe, then at the bottom of the page, Click Close.",
                '                                    ex.Message, MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FileClose(1)
            End If
        End Try
    End Sub
    Public Sub FillTheList()
        'fsw.EnableRaisingEvents = False
        If combItem <> Nothing Then
            If InStr(combItem, "None Yet") > 0 Then
                CheckForFreeSpace()
                Exit Sub
            End If
            Try
                Dim Directory As New System.IO.DirectoryInfo(combItem)
                ListView1.Items.Clear()
                For Each file In Directory.GetFiles()
                    ThisFile = CleanUpFileName(file.Name)
                    If NameWasChanged = True Then
                        Rename(combItem & file.Name, ThisFile)
                        NameWasChanged = False
                    End If
                Next
            Catch ex As Exception

            End Try
            Try
                Dim Directory As New System.IO.DirectoryInfo(combItem)
                ListView1.Items.Clear()
                'TentFile = combItem & "temp" & TentNumber & ".arr"


                ListView1.BeginUpdate()
                WeGotOne = 0
                Dim BlueDuck As Integer = 0
                ' ListView1.View = View.Details
                ' Allow the user to edit item text.
                ListView1.LabelEdit = True
                ListView1.FullRowSelect = True
                ListView1.GridLines = True

                ListView1.ShowItemToolTips = True

                ListView1.Sorting = SortOrder.Ascending
                ' Create two ImageList objects. 
                Dim imageListSmall As New ImageList()
                ' Dim imageListLarge As New ImageList()
                '' Initialize the ImageList objects with bitmaps.
                'dotlocation(i).Image = My.Resources.DarbDot
                imageListSmall.Images.Add(My.Resources.BlueDuck16x16)
                imageListSmall.Images.Add(My.Resources.singleB4_duck)

                ListView1.SmallImageList = imageListSmall



                Dim Stat As Short = 5
                Dim FileSize As Long
                Dim FiSize As Long

                Dim MyStr As String = ""
                'Dim TestEnd As String = "   "
                Dim nextByte As Integer



                Dim counter = My.Computer.FileSystem.GetFiles(combItem)
                FileNumber = counter.Count

                Dim NumbFile As Integer = 0
                Dim item(FileNumber) As ListViewItem

                ListView1.ShowItemToolTips = True


                For Each file In Directory.GetFiles()
                    ' Add a new ListViewItem object, add to the Items collection


                    ThisFile = file.Name
                    'file may be inaccessible
                    ThisFileFull = file.FullName
                    Dim fs As New FileStream(ThisFileFull, FileMode.Open, FileAccess.Read)

                    Dim wholeEnd As String = " "
                    'fs.SetAccessControl(ThisFileFull) = IsNothing()
                    FiSize = file.Length
                    Tail = UCase(Path.GetExtension(ThisFile))
                    If Tail = ".AAA" Then
                        WeGotOne += 1
                        If FiSize >= 10 Then
                            ' Reset the stream's pointer to six spaces from the end

                            fs.Seek(FiSize - 6, SeekOrigin.Begin)
                            nextByte = fs.ReadByte()
                            While (nextByte > 0)
                                ' Console.Write(Convert.ToChar(nextByte))

                                wholeEnd = wholeEnd & Convert.ToChar(nextByte)

                                nextByte = fs.ReadByte()
                            End While

                            'Console.WriteLine()
                            wholeEnd = Trim(wholeEnd)

                        End If
                        '
                        '  sr.Close()
                        'Console.WriteLine("")
                        If Strings.Left(wholeEnd, 3) = "dd&" Then

                            Stat = 0


                            BlueDuck += 1
                        Else
                            Stat = 1

                        End If
                    End If
                    Tail = ""
                    fs.Close()
                    'fs.Dispose()
                    item(NumbFile) = New ListViewItem(ThisFile, Stat)
                    ListView1.ShowItemToolTips = True
                    item(NumbFile).ToolTipText = "To select more than one file, use Ctrl or Shift keys with mouse." & vbCrLf & "Or drag highlight across several files."

                    Stat = 5
                    FileSize = file.Length
                    ListView1.Items.Add(item(NumbFile))


                    MyStr = String.Format("{0:n0}", FileSize)
                    item(NumbFile).SubItems.Add(MyStr)
                    NumbFile += 1
                    'Catch
                    'End Try

                Next

                ListView1.EndUpdate()
                'Me.Controls.Add(ListView1)


                'Dim FileNumber% = ListView1.Items.Count
                'ToolTip4.SetToolTip(ListView1, "To select more than one file, use Ctrl or Shift keys with mouse." & vbCrLf & "Or drag highlight across several files. ------ To open any file in")
                If WeGotOne = 0 And FileNumber = 0 Then
                    Label13.Text = "No Files"
                ElseIf WeGotOne = 0 And FileNumber = 1 Then
                    Label13.Text = "File is Not Encrypted."
                ElseIf WeGotOne = 0 And FileNumber > 1 Then
                    Label13.Text = "No files are Encrypted."
                ElseIf WeGotOne = 1 And FileNumber = 1 Then
                    Label13.Text = WeGotOne% & " file is Encrypted."
                ElseIf WeGotOne = 1 And FileNumber > 1 Then
                    Label13.Text = WeGotOne% & " of " & FileNumber & " files is Encrypted."
                ElseIf WeGotOne > 1 And FileNumber > 1 Then
                    Label13.Text = WeGotOne% & " of " & FileNumber & " files are Encrypted."
                End If

                If BlueDuck = 1 Then
                    Label12.Visible = True
                    Label12.Text = BlueDuck & " is Public Key Encrypted"
                    PictureBox2.Visible = True
                ElseIf BlueDuck > 1 Then
                    Label12.Visible = True
                    Label12.Text = BlueDuck & " are Public Key Encrypted"
                    PictureBox2.Visible = True
                ElseIf BlueDuck = 0 Then
                    Label12.Visible = False
                    PictureBox2.Visible = False
                End If
                CheckForFreeSpace()

                ' Cursor.Current = System.Windows.Forms.Cursors.Default
            Catch
                Kill(OrigDirectory & "away32.ini")
                If ShowingFlashFile = False Then
                    Dim sw As New StreamWriter(whichlist)
                    sw.WriteLine(OrigDirectory & "Demo\")
                    If Directory.Exists(currDrive & "Users") Then
                        sw.WriteLine(currDrive & "Users\" & HisName & "\Documents\")
                    Else
                        sw.WriteLine(currDrive & "Documents and Settings\" & HisName & "\My Documents\")
                    End If
                    sw.Close()

                Else
                    Dim sw As New StreamWriter(whichlist)
                    sw.WriteLine("None Yet")
                    sw.Close()
                End If
                DoTheBox()
            End Try
        Else
            MessageBox.Show("The away32.ini file in " & OrigDirectory & " is corrupted. It is being repaired now and you must restart.", "Bad Initialization File", MessageBoxButtons.OK)
            Kill(OrigDirectory & "away32.ini")
            If ShowingFlashFile = False Then
                Dim sw As New StreamWriter(whichlist)
                sw.WriteLine(OrigDirectory & "Demo\")
                If Directory.Exists(currDrive & "Users") Then
                    sw.WriteLine(currDrive & "Users\" & HisName & "\Documents\")
                Else
                    sw.WriteLine(currDrive & "Documents and Settings\" & HisName & "\My Documents\")
                End If
                sw.Close()

            Else
                Dim sw As New StreamWriter(whichlist)
                sw.WriteLine("None Yet")
                sw.Close()
            End If
            DoTheBox()
        End If

    End Sub
    Sub SpecialFilltheList()
        Try
            Dim Directory As New System.IO.DirectoryInfo(combItem)
            ListView1.Items.Clear()
            'TentFile = combItem & "temp" & TentNumber & ".arr"


            ListView1.BeginUpdate()
            WeGotOne = 0
            Dim BlueDuck As Integer = 0
            ' ListView1.View = View.Details
            ' Allow the user to edit item text.
            ListView1.LabelEdit = True
            ListView1.FullRowSelect = True
            ListView1.GridLines = True

            ListView1.ShowItemToolTips = True

            ListView1.Sorting = SortOrder.Ascending
            ' Create two ImageList objects. 
            Dim imageListSmall As New ImageList()
            ' Dim imageListLarge As New ImageList()
            '' Initialize the ImageList objects with bitmaps.
            'dotlocation(i).Image = My.Resources.DarbDot
            imageListSmall.Images.Add(My.Resources.BlueDuck16x16)
            imageListSmall.Images.Add(My.Resources.singleB4_duck)

            ListView1.SmallImageList = imageListSmall



            Dim Stat As Short = 5
            Dim FileSize As Long
            Dim FiSize As Long

            Dim MyStr As String = ""
            'Dim TestEnd As String = "   "
            Dim nextByte As Integer



            Dim counter = My.Computer.FileSystem.GetFiles(combItem)
            FileNumber = counter.Count

            Dim NumbFile As Integer = 0
            Dim item(FileNumber) As ListViewItem

            ListView1.ShowItemToolTips = True


            For Each file In Directory.GetFiles()
                ' Add a new ListViewItem object, add to the Items collection


                ThisFile = file.Name
                'file may be inaccessible
                ThisFileFull = file.FullName
                Dim fs As New FileStream(ThisFileFull, FileMode.Open, FileAccess.Read)

                Dim wholeEnd As String = " "
                'fs.SetAccessControl(ThisFileFull) = IsNothing()
                FiSize = file.Length
                Tail = UCase(Path.GetExtension(ThisFile))
                If Tail = ".AAA" Then
                    WeGotOne += 1
                    If FiSize >= 10 Then
                        ' Reset the stream's pointer to six spaces from the end

                        fs.Seek(FiSize - 6, SeekOrigin.Begin)
                        nextByte = fs.ReadByte()
                        While (nextByte > 0)
                            ' Console.Write(Convert.ToChar(nextByte))

                            wholeEnd = wholeEnd & Convert.ToChar(nextByte)

                            nextByte = fs.ReadByte()
                        End While

                        'Console.WriteLine()
                        wholeEnd = Trim(wholeEnd)

                    End If
                    '
                    '  sr.Close()
                    'Console.WriteLine("")
                    If Strings.Left(wholeEnd, 3) = "dd&" Then

                        Stat = 0


                        BlueDuck += 1
                    Else
                        Stat = 1

                    End If
                End If
                Tail = ""
                fs.Close()
                'fs.Dispose()
                item(NumbFile) = New ListViewItem(ThisFile, Stat)
                ListView1.ShowItemToolTips = True
                item(NumbFile).ToolTipText = "To select more than one file, use Ctrl or Shift keys with mouse." & vbCrLf & "Or drag highlight across several files."

                Stat = 5
                FileSize = file.Length
                ListView1.Items.Add(item(NumbFile))


                MyStr = String.Format("{0:n0}", FileSize)
                item(NumbFile).SubItems.Add(MyStr)
                NumbFile += 1
                'Catch
                'End Try

            Next

            ListView1.EndUpdate()
            'Me.Controls.Add(ListView1)


            'Dim FileNumber% = ListView1.Items.Count
            'ToolTip4.SetToolTip(ListView1, "To select more than one file, use Ctrl or Shift keys with mouse." & vbCrLf & "Or drag highlight across several files.")
            Label13.Text = WeGotOne% & " of " & FileNumber & " files are Encrypted."
            If BlueDuck = 1 Then
                Label12.Visible = True
                Label12.Text = BlueDuck & " is Public Key Encrypted"
                PictureBox2.Visible = True
            ElseIf BlueDuck > 1 Then
                Label12.Visible = True
                Label12.Text = BlueDuck & " are Public Key Encrypted"
                PictureBox2.Visible = True
            ElseIf BlueDuck = 0 Then
                Label12.Visible = False
                PictureBox2.Visible = False
            End If
            CheckForFreeSpace()

            ' Cursor.Current = System.Windows.Forms.Cursors.Default
        Catch
            Kill(OrigDirectory & "away32.ini")
            If ShowingFlashFile = False Then
                Dim sw As New StreamWriter(whichlist)
                sw.WriteLine(OrigDirectory & "Demo\")
                If Directory.Exists(currDrive & "Users") Then
                    sw.WriteLine(currDrive & "Users\" & HisName & "\Documents\")
                Else
                    sw.WriteLine(currDrive & "Documents and Settings\" & HisName & "\My Documents\")
                End If
                sw.Close()

            Else
                Dim sw As New StreamWriter(whichlist)
                sw.WriteLine("None Yet")
                sw.Close()
            End If
            DoTheBox()
        End Try
    End Sub
    Sub CheckForFreeSpace()
        Dim abbrev As String = ""
        Dim abbreAmount As Double

        If ShowingFlashFile = True Then
            GetFreeSpace(FlashDriveLetter)
        Else
            GetFreeSpace(currDrive)
        End If


        Select Case lFreeBytes
            Case Is > 999999999
                abbrev = "G"
                abbreAmount = lFreeBytes \ 1000000000
            Case Is > 999999, Is < 999999999
                abbrev = "M"
                abbreAmount = lFreeBytes \ 1000000
            Case Is > 999, Is < 999999
                abbrev = "k"
                abbreAmount = lFreeBytes \ 1000
        End Select
        Dim MineStr = String.Format("{0:n0}", lFreeBytes)
        Label1.Text = "Drive Free Space = " + MineStr + "    (" & abbreAmount & abbrev & "B)"
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        If Boxing = False Then
            If ShowingFlashFile = False Then
                combItem = CStr(ComboBox1.SelectedItem)
            Else
                combItem = FlashDriveLetter & CStr(ComboBox1.SelectedItem)
            End If

            FillTheList()
        End If
    End Sub

    Public Sub ListView1_DoubleClick(sender As Object, e As EventArgs) Handles ListView1.DoubleClick
        'fsw.EnableRaisingEvents = False
        'Dim IRet&
        'If GoViewer = 0 Then
        '    RunDouble = 1
        '    Prepare()
        '    RunDouble = 0
        'End If
        'If Gotcha And GoViewer = 0 Then              'DECRYPT THE FILE
        'ListView1.GetItemAt()


        Dim lvi As ListViewItem = Me.ListView1.SelectedItems(0)

        ThisFile = lvi.SubItems(0).Text
        GoViewer = True
        Tail = UCase(Path.GetExtension(ThisFile))
        If Tail = ".AAA" Then
            MessageBox.Show("You need to Decrypt this file first. Or try 'View Image or Text File'", "Encrypted File", MessageBoxButtons.OK)
            '    msg$ = "Other applications can not work with Encrypted files. You must Decrypt the file first." _
            '    & vbCrLf & "Shall we do that?"
            '    Style = vbYesNo           ' Define buttons.
            '    Title$ = "Encrypted File"
            '    Response = MsgBox(msg$, Style, Title$)

            '    If Response = 6 Then
            '        'chkAutoEncrypt_Click
            '        cmddecrypt_Click()
            '    End If
        ElseIf Tail = ".RTF" Then
            'Standard Encrypt AwayRJNWriter note

            Cmd = "ENCRYPT"


            RealName = ThisFile
            dlgPrivatePasswordVisible.ShowDialog()
            If ComingBack = True Then
                ComingBack = False
                GoViewer = False
                Exit Sub
            End If
            StandardNoteEncrypting = True
            frmMainWriter.LinkLabel2.Visible = False
            frmMainWriter.doc.LoadFile(combItem & ThisFile)
            frmMainWriter.doc.Select(frmMainWriter.doc.Text.Length, 0)
            frmMainWriter.TextBox1.Select(frmMainWriter.Text.Length, 0)
            frmMainWriter.ShowDialog()
            If ComingBack = True Then
                ComingBack = False
                GoViewer = False
                Exit Sub
            End If
            DoTheNoteEmail()
            GoViewer = False
        Else
            GoViewer = False
            Try
                Process.Start(combItem & ThisFile)
            Catch
                MessageBox.Show("You do not have the application to do " & ThisFile, "No Application", MessageBoxButtons.OK)

            End Try

        End If

        'fsw.EnableRaisingEvents = True
    End Sub

    Private Sub Picturebox1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDown
        Dim PIK As Boolean
        PIK = False
        ScrubMemory(InternalKey, PIK)
        PIK = False
        ScrubMemory(PrivatePassword, PIK)
        PIK = False
        ScrubMemory(PublicKey, PIK)
        PIK = False
        ScrubMemory(NonceKey, PIK)
        PIK = True
        ScrubMemory(PaddedInternalKey, PIK)

        InternalKey = ""
        PrivatePassword = ""
        PublicKey = ""
        NonceKey = ""
        PaddedInternalKey = ""

        GreenLight()
        PasswordInRamTimer.Enabled = False
        GreenLight()

    End Sub
    Sub GreenLight()
        Dim gr As Graphics = PictureBox1.CreateGraphics
        gr.FillEllipse(Brushes.Green, 10, 11, 9, 9)
        gr.Dispose()
        ToolTip2.SetToolTip(PictureBox1, "No Password in Memory.")
        PasswordInRamTimer.Enabled = False
    End Sub
    Private Sub PasswordInRamTimer_Tick(sender As Object, e As EventArgs) Handles PasswordInRamTimer.Tick
        Static toggling As Boolean
        If toggling = True Then
            toggling = False
            Dim gr As Graphics = PictureBox1.CreateGraphics
            gr.FillEllipse(Brushes.Red, 10, 11, 9, 9)
            gr.Dispose()
        Else
            toggling = True
            Dim gr As Graphics = PictureBox1.CreateGraphics
            gr.FillEllipse(Brushes.DarkSalmon, 10, 11, 9, 9)
            gr.Dispose()
            'ToolTip1.SetToolTip(Button1, "Save changes")
            ToolTip2.SetToolTip(PictureBox1, "Password in Memory - Click to Clear.")
        End If
    End Sub

    Private Sub Button20_Click(sender As Object, e As EventArgs)


    End Sub

    'for star up only

    Private Sub PictureBox1_Paint(sender As Object, e As PaintEventArgs) Handles PictureBox1.Paint
        e.Graphics.FillEllipse(Brushes.Green, 10, 11, 9, 9)
        e.Dispose()
        ToolTip2.SetToolTip(PictureBox1, ("No Password in Memory."))
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs)

    End Sub
    Private Sub Button9_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub Button13_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub ExitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExitToolStripMenuItem.Click
        dismount()
    End Sub

    'Private Sub openNotepd_Click(sender As Object, e As EventArgs) Handles openNotepd.Click
    '    'fsw.EnableRaisingEvents = False
    '    Dim ntpd As String, ntpdVal As Integer
    '    Try
    '        If currDrive <> "C:\" Then
    '            Stop
    '        End If
    '        ntpd = currDrive & "Windows\notepad.exe"
    '        ntpdVal = Shell(ntpd, AppWinStyle.NormalFocus)
    '    Catch ex As Exception
    '        Dim eventLog As EventLog = New EventLog
    '        Dim SourceName As String = "WindowsService.ExceptionLog"
    '        If Not eventLog.SourceExists(SourceName) Then
    '            eventLog.CreateEventSource(SourceName, "AWAYRJN")
    '        End If

    '        eventLog.Source = SourceName
    '        Dim message As String = String.Format("Exception: {0} " & "currdrive = " & currDrive & "*" & vbLf & vbLf & vbCrLf & "Stack: {1}", ex.Message, ex.StackTrace)
    '        eventLog.WriteEntry(message, EventLogEntryType.Error)
    '    End Try

    '    'fsw.EnableRaisingEvents = True
    'End Sub
    Public Function GetFreeSpace(ByVal Drive As String) As Long

        'Dim iAns As Long
        Dim cdrive As System.IO.DriveInfo
        cdrive = My.Computer.FileSystem.GetDriveInfo(currDrive)
        lFreeBytes = cdrive.TotalFreeSpace
        'iAns = GetDiskFreeSpaceEx(Drive, lFreeBytesAvailable, _
        '     lBytesTotal, lFreeBytes)
        If lFreeBytes > 0 Then

            Return lFreeBytes
        Else
            Throw New Exception("Invalid or unreadable drive")
        End If


    End Function


    Private Sub AboutAwayRJNCryptographyToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AboutAwayRJNCryptographyToolStripMenuItem.Click
        AboutBox1.Show()
    End Sub

    Private Sub GettingStartedAndEverythingElseToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GettingStartedAndEverythingElseToolStripMenuItem.Click
        Dim IRet As Integer
        On Error GoTo mnuTopics_Click_Error

        'On Error Resume Next


        'On Error GoTo 0
        'If AutoEncryptTimerStillDoing Then
        '    Exit Sub
        'End If
        'If NeedAutoEncrytTimer Then
        '    chkAutoEncrypt.Value = 0
        '    chkAutomaticDecrypt.Value = 0
        'End If

        Dim webAddress As String = "http://www.away32.com/helpfile/Away_RJN_Crypto_Help.html"

        Process.Start(webAddress)

        'Exit Sub

        On Error GoTo 0
        Exit Sub

mnuTopics_Click_Error:

        MsgBox("Error " & Err.Number & " (" & Err.Description & ") in procedure GettingStartedAndEverythingElseToolStripMenuItem_Click of Form frmAWAY32")
        On Error GoTo 0
    End Sub


    Private Sub GenerateUniquePasswordsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GenerateUniquePasswordsToolStripMenuItem.Click
        'If AutoEncryptTimerStillDoing Then
        '    Exit Sub
        'End If
        'If NeedAutoEncrytTimer Then
        '    chkAutoEncrypt.Value = 0
        '    chkAutomaticDecrypt.Value = 0
        'End If
        frmRandomPassword.Show()
    End Sub



    'Private Sub chkPassword_CheckedChanged(sender As Object, e As EventArgs)
    '    Dim passw$

    '    passw$ = Str(chkPassword.Checked)
    '    SaveSetting("winexpl", "startup", "chkPassword", passw$)
    'End Sub
    Public Sub ChkEncryptFilenames_CheckedChanged(sender As Object, e As EventArgs) Handles chkEncryptFilenames.CheckedChanged
        If chkEncryptFilenames.CheckState = CheckState.Checked Then
            EncrypFileName = True
        Else
            EncrypFileName = False
        End If



        SaveSetting("winexpl", "startup", "chkEncryptFilenames", CStr(EncrypFileName))
    End Sub

    Private Sub ChkAutoEncrypt_CheckedChanged(sender As Object, e As EventArgs) Handles chkAutoEncrypt.CheckedChanged
        If AutoEncryptTimerStillDoing Then
            AutoEncryptTimer.Enabled = False
            NeedAutoEncrytTimer = False
            QuitProcessing = True
            chkAutoEncrypt.Checked = False
            'chkAutomaticDecrypt.Value = 0
            Cancelled = True

            Exit Sub
        End If
        If chkAutoEncrypt.Checked Then
            'chkAutomaticDecrypt.Value = 0
            chkAutoEncrypt.Text = "Automatic Encrypt Active"
            chkAutoEncrypt.Font = New Font(chkAutoEncrypt.Font, FontStyle.Bold)
            'chkAutoEncrypt.Font.Bold = True
            'chkAutoEncrypt.ForeColor = &H8000&
            'chkAutomaticDecrypt.Enabled = False

        Else
            ' chkAutomaticDecrypt.Enabled = True
            chkAutoEncrypt.Text = "Automatic Encrypt"
            chkAutoEncrypt.Font = New Font(chkAutoEncrypt.Font, FontStyle.Regular)
            'chkAutoEncrypt.FontBold = False
            'chkAutoEncrypt.ForeColor = &H80000012

        End If
        If NeedAutoEncrytTimer = False Then
            Cancelled = False
            GettingAutoEncryptPassword = True
            AutoEncryptTimer.Enabled = False
            Cmd$ = "ENCRYPT"
            'TypeofKey()
            If Cancelled Then
                Exit Sub
            Else
                AutoEncryptTimer.Enabled = True
                NeedAutoEncrytTimer = True
                GettingAutoEncryptPassword = False
                QuitProcessing = False
            End If
        Else
            AutoEncryptTimer.Enabled = False
            NeedAutoEncrytTimer = False

        End If
        Exit Sub
    End Sub


    Private Sub AutoEncryptTimer_Tick(sender As Object, e As EventArgs) Handles AutoEncryptTimer.Tick
        Dim efl As Integer
        AutoEncryptTimer.Enabled = False
        If NeedAutoEncrytTimer = False Then
            AutoEncryptTimerStillDoing = False
            Exit Sub
        End If
        AutoEncryptTimerStillDoing = True
        On Error GoTo exitLoop
        ' EncryptingLabel.Caption = "Searching"
        '        For efl = 1 To FolderNumber
        '            Pat$ = ExistingFolders$(efl)
        '            'Cmd$ = "ENCRYPT"
        '            GoViewer = 0
        '            AutoEncryptionSequence = True
        '            Prepare()
        '            AutoEncryptTimer.Enabled = False
T3101:
        '        Next efl
        AutoEncryptTimer.Enabled = True
        'EncryptingLabel.Caption = ""
        AutoEncryptTimerStillDoing = False
        AutoEncryptionSequence = False
        On Error GoTo 0
        Exit Sub
exitLoop:
        Resume T3101

    End Sub



    Private Sub Label14_Click(sender As Object, e As EventArgs) Handles Label14.Click
        Process.Start("http://www.away32.com/rijndaelaesAG.html")
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs)

    End Sub



    Private Sub Button3_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub ToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem2.Click
        '---------------------------------------------------------------------------------------
        ' Procedure : mnuShowFlash_Click
        ' DateTime  : 4/23/2006 18:19 vb6            8/30/2013 vb.net
        ' Author    : Richard L
        ' Purpose   :
        '---------------------------------------------------------------------------------------


        Select Case mnuFlashDriveStatus$
            Case "No Flash Inserted"
                FindFlash()              'Make sure
                If FlashDrive = False Then
                    ToolStripMenuItem2.Text = "Click to Show Special Folders on Flash Drive"
                    Exit Sub
                    'Else
                    '                    mnuShowFlash.Caption = "Show Flash Drive Special Folders"
                    '                    mnuFlashDriveStatus$ = "Available"
                    '                    mnuShowFlash_Click()
                End If
            Case "Available"
                ToolStripMenuItem2.Checked = True
                mnuFlashDriveStatus$ = "Available and Clicked"
                whichlist$ = FlashLocation$
                Label2.Text = "Special Folders on Removable Drive " & Strings.Left(FlashLocation$, 3)
                'mnuShowFlash.Caption = "Show Flash Drive Special Folders"
                ShowingFlashFile = True
                'ShowFlashFolder()
                DoTheBox()
                Exit Sub
            Case "Available and Clicked"
                ToolStripMenuItem2.Checked = False
                FindFlash()
                Label2.Text = "Special Folders"
                ShowingFlashFile = False
                DoTheBox()
                Exit Sub
        End Select

    End Sub

    Public Sub FindFlash()
        findingFlashTimer.Enabled = False
        'mnuShowFlash.Checked = False
        'mnuShowFlash.Enabled = False
        'mnuShowFlash.Visible = False

        FlashDrive = False
        mnuFlashDriveStatus = "No Flash Inserted"
        FoundFlashAlready = False
        Dim drives = DriveInfo.GetDrives()

        For Each drive As DriveInfo In drives

            ' skip unknown drives
            If (drive.DriveType = DriveType.Unknown) Then
                Continue For
            End If
            If drive.DriveType = DriveType.Removable Then
                FlashDriveLetter = drive.Name
                If FlashDriveLetter <> "A:\" Then
                    If FlashDriveLetter <> "B:\" Then
                        Try
                            Dim sw As New StreamWriter(FlashDriveLetter & "FlashHash.txt")
                            sw.Close()

                            Kill(FlashDriveLetter & "FlashHash.txt")
                            FlashDrive = True
                            FlashLocation$ = FlashDriveLetter & "flashlist.ini"
                            FoundFlashAlready = True ' should be after it has been selected
                            mnuFlashDriveStatus = "Available"
                            'mnuShowFlash.Checked = False
                            'mnuShowFlash.Enabled = True
                            'mnuShowFlash.Visible = True
                            Exit For
                        Catch ex As Exception
                            Continue For
                        End Try
                    End If
                End If
            End If
        Next

        findingFlashTimer.Enabled = True
    End Sub

    Private Sub FindingFlashTimer_Tick(sender As Object, e As EventArgs) Handles findingFlashTimer.Tick
        '    '---------------------------------------------------------------------------------------
        '    ' Procedure : findingFlashTimer
        '    ' DateTime  : 6/18/2006 11:27      8/30/2013
        '    ' Author    : Richard L. Bennice
        '    ' Purpose   :
        '    '---------------------------------------------------------------------------------------
        '    '
        If FoundFlashAlready = True Then
            Exit Sub
        End If
        FindFlash()


    End Sub











    Private Sub Button5_Click(sender As Object, e As EventArgs)
        'delete files
        'fsw.EnableRaisingEvents = False
        Dim counter = My.Computer.FileSystem.GetFiles(combItem)
        Dim FileNumber As Integer = counter.Count
        Dim FileToBeDeleted(FileNumber) As String

        ''            TurnOffButtons()

        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If

        '        'ListView1.HideSelection = False
        '        If NeedAutoEncrytTimer Then
        '            chkAutoEncrypt.Value = 0
        '            chkAutomaticDecrypt.Value = 0
        '        End If

        If combItem <> "None Yet" Then
            Cursor.Current = System.Windows.Forms.Cursors.WaitCursor
            Dim FileBooleanMarked(FileNumber) As Boolean
            Dim Status As Integer
            Dim selectedCount As Integer
            For i As Integer = ListView1.SelectedIndices.Count - 1 To 0 Step -1
                Status = ListView1.SelectedIndices.Item(i)
                FileBooleanMarked(Status) = True
                selectedCount += 1
            Next
            Dim SmalCount As Integer = 0
            For itmX = 0 To FileNumber
                If FileBooleanMarked(itmX) = True Then
                    Dim lvi As ListViewItem = Me.ListView1.SelectedItems(SmalCount)
                    SmalCount += 1
                    FileToBeDeleted(SmalCount) = combItem & lvi.SubItems(0).Text
                End If
            Next
            FileClose()

            For jx = 1 To SmalCount
                SmashPicture(FileToBeDeleted(jx))
            Next

            '            'istView1.HideSelection = False
            '            msg$ = "Scramble/Delete Highlighted Files?"
            '            Style% = vbYesNo          ' Define buttons.
            '            Title$ = "Deleting Files From Special Folder"
            '            ' Display message.
            '            Response = MsgBox(msg$, Style%, Title$)
            '            'ListView1.HideSelection = True
            '            If Response <> vbYes Then ' User chose Yes.

            '                frmAWAY32.FillTheList()
            '                Exit Sub
            '            End If
            '            Dim itmX As ListItem
            '            Screen.MousePointer = vbHourglass
            '            

            '            frmAWAY32.FillTheList()
            '            frmAWAY32.Show()
            '            Screen.MousePointer = vbDefault

            FillTheList()
        End If
        'Cursor.Current = System.Windows.Forms.Cursors.Default
        'fsw.EnableRaisingEvents = True
        '        Exit Sub
        'SelectFil:
        '        Resume Next
        '        frmAWAY32.GetStarted()
        '        Exit Sub
    End Sub





    'Private Sub FileSystemWatcher1_Created(sender As Object, e As FileSystemEventArgs)
    '    FillTheList()
    'End Sub


    Sub Prepare()
        Try
            Dim kj As Integer
            'Dim tem As String, bytTem() As Byte, emailLength$()
            Dim HighLightedFile As Integer = 0         ', FilesExtension() As String
            'Dim BackSlash$
            'Dim PubDirFiles As Integer

            Dim cunt As Integer = 0, Gotcha As Integer = 0
            'Pat$ = flbList1.Path
            'Close()
            'If frmProcessFiles.WindowState = vbMinimized Then
            '    frmProcessFiles.WindowState = vbNormal
            'End If
            'If InStr(combItem, "None Yet") = 0 And FileNumber > 0 Then
            ReDim FileToBeEncrypted(FileNumber)


            ' Folder2ShowinCombo1Box$ = Combo1.Text

            'If File.Exists(combItem & strippedPEtemp) Then
            '    Kill(combItem & strippedPEtemp)
            '    FileNumber -= 1
            'End If

            ReDim FileNames(FileNumber)
            'ReDim FilesExtension(FileNumber)




            FileNumber = 0

            '*********************PICK AND CHOOSE **********
            Dim HighlightedGroup As ListView.SelectedListViewItemCollection = Me.ListView1.SelectedItems
            Dim item As ListViewItem
            For Each item In HighlightedGroup

                HighLightedFile = HighLightedFile + 1
                FileToBeEncrypted(cunt + 1) = True
                FileNames(HighLightedFile) = item.Text

                FileNumber += 1
                'Else
                'FileToBeEncrypted(cunt + 1) = False
                'End If
                cunt += 1
            Next

            '********************CHOOSE EM ALL ***********************

            If HighLightedFile = 0 Then
                cunt = 0

                For Each item In ListView1.Items
                    FileToBeEncrypted(cunt + 1) = True
                    FileNames(cunt + 1) = item.Text
                    cunt += 1
                Next

                FileNumber = cunt
            End If

            'Dim testname(FileNumber, 2) As String
            'testname(1, 1) = "testtteedd"
            'testname(1, 2) = "publicEncerypted"
            If EncryptingFileWithSignature = True Then
                EncryptingFileWithSignature = False
                Exit Sub
            End If





            DecryptFromPrivateKey = False

            'Kill(PublicDirectory$ & "\*.*")
            ', bUUNN As Byte = CByte("   ")
            ReDim AnalysedFile(FileNumber, 4)
            PreviousSessionNumber = ""
            Dim controlNumber() As Integer, test As String = ""
            ReDim controlNumber(FileNumber)
            'controlNumber(1) = 4
            'test = FileNames(controlNumber(1))

            If RandomViewing = True And GoViewer = True Then
                Dim MyRand As New Random(CInt(DateDiff("s", "5/10/1968", Now)))
                Dim RememberSet As New HashSet(Of Integer)
                Dim RandomNumber As Integer
                'Type:           System.Boolean()
                'true if the element is added to the HashSet(Of T) object; false if the element is already present.
                For j = 1 To FileNumber
                    RandomNumber = MyRand.Next(1, FileNumber + 1)
                    If RememberSet.Add(RandomNumber) = True Then
                        controlNumber(j) = RandomNumber
                    Else
                        j -= 1
                    End If
                Next j
            Else
                For m = 1 To FileNumber
                    controlNumber(m) = m
                Next m
            End If
            DarbyCode = False
            For kj = 1 To FileNumber

                FileLength = CInt(FileLen(combItem & FileNames(kj)))

                AnalysedFile(kj, 0) = combItem & FileNames(kj)

                'Dim bUUNN As Byte
                RealName = FileNames(kj)
                Dim pathSource As String = combItem & FileNames(kj)
                If FileLen(pathSource) < 25 Then
                    GoTo NextKJNumber
                End If
                'MessageBox.Show("inside kj to file - " & kj, "choose", MessageBoxButtons.OK)
                Try      'Identify the File Type
                    Using fs As New FileStream(pathSource, FileMode.Open, FileAccess.Read)

                        Dim wholeEnd As String = " "
                        Dim nextByte As Integer
                        fs.Seek(FileLength - 6, SeekOrigin.Begin)
                        nextByte = fs.ReadByte()
                        While (nextByte > 0)
                            wholeEnd = wholeEnd & Convert.ToChar(nextByte)
                            nextByte = fs.ReadByte()
                        End While
                        'MessageBox.Show("inside using fs", "choose", MessageBoxButtons.OK)
                        wholeEnd = Trim(wholeEnd)
                        If FileLength > 25 Then
                            UUnn$ = Strings.Left(wholeEnd, 3)
                            SavedLengthFileName = Strings.Mid(wholeEnd, 4, 3)
                            AnalysedFile(kj, 1) = SavedLengthFileName
                            AnalysedFile(kj, 2) = UUnn

                        End If
                    End Using

                Catch
                    Stop
                End Try
                Select Case UUnn
                    Case "jP$", "jP*", "jj$", "jj*", "rr&", "##&", "ff&", "%%&", "ss&", "vv&", "tj*", "tP*", "jPk", "jjk", "tPk", "tjk", "jPL", "jjL"
                        'P = Darb encrypt
                        '  rr&  = Orig Rijndael file
                        'jPl & jjk =viewable pics
                        '"jj*" 'Viewable (GIF, JPG etc) Rijndael file
                        '"jj$" 'NON viewable Rijndael file
                        ' jPl & jjL NON viewable file
                        'ss& = Old Defunct Public Encrypted!!!   Now using dd&
                        'tPk,tjk = RJN Writer (richtextFormat)RTF
                        Gotcha = 1
                        DecryptFromPrivateKey = False
                        PublicEncrypted = False
                        If Strings.Right(UUnn, 2) = "Pk" Or Strings.Right(UUnn, 2) = "PL" Then
                            DarbyCode = True
                        Else
                            DarbyCode = False
                        End If
                        If Strings.Right(UUnn, 1) = "k" Or Strings.Right(UUnn, 1) = "L" Then
                            AnalysedFile(kj, 4) = CStr(12)      ' BlankingLength = 12
                        Else
                            AnalysedFile(kj, 4) = CStr(0)      ' BlankingLength = 0
                        End If
                        'AnalysedFile(kj, 1) = SavedLengthFileName     efficient way!!!!
                        'AnalysedFile(kj, 2) = UUnn
                        AnalysedFile(kj, 3) = "RegEncrypted"
                        'AnalysedFile(kj, 1, 1, 0) = SavedLengthFileName    ORIGINAL WAY!!!!
                        'AnalysedFile(kj, 1, 1, 1) = UUnn    WASTEFUL!!!!!
                    Case "dd&"
                        If InStr(AnalysedFile(kj, 0), "stripPE.txt") = 0 Then
                            PublicEncrypted = True
                            DecryptFromPrivateKey = True
                            AnalysedFile(kj, 3) = "PublicEcrypted"
                            AnalysedFile(kj, 4) = CStr(0)      ' BlankingLength = 0 because shortening automatically takes out blanking
                            If PubEncrFile = 0 Then
                                PubEncrFile = kj
                            End If
                            'DarbyCode = False
                            'pathSource = AnalysedFile(kj, 0)
                            'pathSource = combItem & RealName

                        Else
                            AnalysedFile(kj, 3) = "PlainText"
                            AnalysedFile(kj, 4) = CStr(0)      ' BlankingLength = 0
                            'DarbyCode = False
                        End If

                    Case Else
                        AnalysedFile(kj, 3) = "PlainText"
                        ' DarbyCode = False
                End Select
NextKJNumber:
            Next kj


            If CBool(Gotcha Or CInt(DecryptFromPrivateKey = True) Or CInt(Cmd = "ENCRYPT")) Then
                If CInt(AutoEncryptionSequence) = 0 Then
                    If DecryptFromPrivateKey = True Then
                        GetSendersEmailSessionKey(PubEncrFile)     '****************** OPENS SESSION KEY
                    End If
                    TypeofKey()

                    Cursor.Current = System.Windows.Forms.Cursors.WaitCursor
                    'If ComingBack = 0 Then
                    '    frmProcessFiles.Show()
                    'End If
                End If

                If ComingBack Then
                    ComingBack = False
                    Cursor.Current = System.Windows.Forms.Cursors.Default
                    Exit Sub
                End If
            End If
starttheLoop:
            For kj = 1 To FileNumber
                FileLength = CInt(FileLen(combItem & FileNames(controlNumber(kj))))
                If FileLength < 25 Then
                    GoTo NextKJSecondTime
                End If
                Select Case AnalysedFile(controlNumber(kj), 3)
                    'Select Case UUnn
                    Case "RegEncrypted"

                        Gotcha = 1
                        If Cmd = "DECRYPT" Then
                            If FileNumber > 2 Then

                                ProgressBar2.Visible = True
                                ProgressBar2.Maximum = FileNumber
                                ProgressBar2.Value = kj
                            End If

                            'If Len(PrivatePassword) = 32 Then
                            '    Dim testPrivPass As String ', testPrivPass1 As String, testPrivPass2 As String
                            '    testPrivPass = Hash.HexFromString(PrivatePassword, HashAlgorithm.Sha256)

                            GenerateInternalKey(PrivatePassword$)
                            PasswordInRamTimer.Enabled = True
                            DoingPublic = False


                            'AnalysedFile(kj, 1) = SavedLengthFileName
                            'AnalysedFile(kj, 2) = UUnn
                            'AnalysedFile(kj, 3) = "RegEncrypted"
                            'AnalysedFile(kj, 4) = "Blanking Length" 
                            SavedLengthFileName = AnalysedFile(controlNumber(kj), 1)
                            RealName = FileNames(controlNumber(kj))
                            FileToBeEncrypted(controlNumber(kj)) = True
                            Encrypt(Cmd$, combItem, RealName, controlNumber(kj))
                            If DarbyCode = True Then
                                PrivatePassword = ""
                            End If
                        End If
                    Case "PublicEcrypted"
                        'Case "dd&"               ', "dP&"      'PUBLIC KEY ENCRYPTED

                        'On Error Resume Next

                        'Dim chuk As Integer, strTemp As String, PubDirFileLocation As Long, Chunk&, Blnkg%
                        'Dim NumberOfChunks%, LastChunk&, ChunkLocation&, Blanking$, amaiL%, strSess%
                        'FileLength = FileLen(ActualPath$ & BackSlash & FileNameskj))

                        ' On Error GoTo 0
                        If GoViewer = True Then
                            GoViewer = False
                            Dim msges As String = "At least one of the Files you have selected for Viewing is Encrypted with a Public Key." & vbCrLf & vbCrLf & RealName & vbCrLf & vbCrLf & "You need to Decrypt the File first using the appropriate Session Key. "
                            Dim response = MessageBox.Show(msges, "Public Encrypted File", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)


                            If response = vbOK Then    ' User chose Yes.
                                GotAPublicKey = True
                                'TurnOnButtons()
                                FillTheList()
                                Exit Sub
                            End If
                        End If
                        DecryptFromPrivateKey = True
                        'FileLength = CInt(FileLen(combItem & FileNames(kj)))
                        DoingPublic = True
                        'GetSendersEmailSessionKey(kj)
                        RealName = FileNames(controlNumber(kj))
                        pathSource = combItem & RealName
                        'AnalysedFile(controlNumber(kj), 1) = SavedLengthFileName
                        'AnalysedFile(controlNumber(kj), 2) = UUnn
                        'AnalysedFile(controlNumber(kj), 3) = "RegEncrypted"
                        SavedLengthFileName = AnalysedFile(controlNumber(kj), 1)
                        'fsSource.Close()

                        If ComingBack = True Then
                            ComingBack = False
                            Cursor.Current = System.Windows.Forms.Cursors.Default
                            Exit Sub
                        End If

                        If GetNewSessionInfo = True Then
                            'FileLength = CInt(FileLen(pathSource))
                            Dim chuk As Integer
                            Dim ChunkLocation As Integer ', PubDirFileLocation As Integer
                            SetUpChunks(FileLength)   'No need to comp for B64 length
                            ChunkLocation = 0
                            'PubDirFiles = PubDirFiles + 1
                            '            PubDirFilesName(PubDirFiles) = FileNameskj)
                            '*******************************'NOW REDUCE THE GODDAMN FILE BY THat AMOUNT!!!!!**************

                            If File.Exists(combItem & strippedPEtemp) Then
                                Kill(combItem & strippedPEtemp)
                            End If
                            For chuk = 1 To NumberOfChunks
                                If chuk = NumberOfChunks Then
                                    Chunk = LastChunk
                                End If

                                Dim FileHolder As String = ""
                                Using fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
                                    Dim BytFileHolder() As Byte = New Byte(Chunk - 1) {}
                                    numBytesToRead = Chunk
                                    numBytesRead = 0
                                    fsSource.Seek(ChunkLocation, SeekOrigin.Begin)       'Getting Chunk Size FileHolder
                                    While (numBytesToRead > 0)

                                        Dim nx As Integer = fsSource.Read(BytFileHolder, numBytesRead, numBytesToRead)
                                        If (nx = 0) Then
                                            Exit While
                                        End If
                                        numBytesRead = (numBytesRead + nx)
                                        numBytesToRead = (numBytesToRead - nx)
                                    End While

                                    FileHolder = System.Text.Encoding.UTF8.GetString(BytFileHolder)

                                End Using

                                Dim strTemp As String = ""

                                If chuk = 1 Then
                                    '                         9    +      3 + 20                ;4   ;    10        = 46 - SendersEMail$ = 26
                                    ' strEncryptedFile$ = BlankingSTR & SendersB64 & strSessionNumber & SessionID & strEncryptedFile$

                                    Using sw2 As New StreamWriter(combItem & strippedPEtemp)
                                        strTemp = Strings.Mid(FileHolder, CInt(LenmaiL) + 27)
                                        sw2.Write(strTemp, combItem & strippedPEtemp)

                                    End Using

                                Else
                                    Using sw2 As New StreamWriter(combItem & strippedPEtemp, append:=True)

                                        sw2.Write(FileHolder)

                                    End Using
                                End If

                                ChunkLocation = ChunkLocation + Chunk
                            Next chuk
                            'fsSource.Close()

                            DecryptPublicEncryptedFile(combItem, strippedPEtemp, controlNumber(kj))
                        Else
                            MsgBox("One of these files is from another email session and cannot be decrypted yet. Run again after completion.") 'need new session number
                        End If
                    Case "PlainText"

                        ' If RunDouble Then
                        Gotcha = 0
                        If Cmd = "ENCRYPT" Then
                            If FileNumber > 2 Then

                                ProgressBar2.Visible = True
                                ProgressBar2.Maximum = FileNumber
                                ProgressBar2.Value = kj

                            End If
                            GenerateInternalKey(PrivatePassword$)

                            PasswordInRamTimer.Enabled = True
                            FileToBeEncrypted(controlNumber(kj)) = True
                            RealName = FileNames(controlNumber(kj))
                            Encrypt(Cmd$, combItem, RealName, controlNumber(kj))
                            If DarbyCode = True Then
                                PrivatePassword = ""
                            End If
                        End If
                        Dim TextPlainText As String = UCase(Path.GetExtension(CStr(FileNames(controlNumber(kj)))))
                        If GoViewer = True Then
                            Select Case TextPlainText
                                Case ".RTF"
                                    Cmd = "ENCRYPT"
                                    StandardNoteEncrypting = True
                                    frmMainWriter.LinkLabel2.Visible = False
                                    frmMainWriter.doc.LoadFile(combItem & FileNames(controlNumber(kj)))
                                    frmMainWriter.doc.Select(frmMainWriter.doc.Text.Length, 0)
                                    frmMainWriter.TextBox1.Select(frmMainWriter.Text.Length, 0)
                                    RealName = FileNames(controlNumber(kj))
                                    frmMainWriter.ShowDialog()
                                    If ComingBack = True Then
                                        ComingBack = False
                                        Exit Sub
                                    End If
                                    DoTheNoteEmail()
                                    GoViewer = False
                                Case ".TXT"
                                    ComingBack = True
                                    Exit Sub
                                Case Else

                                    RealName = FileNames(controlNumber(kj))
                                    pathSource = combItem & RealName

                                    XPBackground.Text = pathSource
                                    Using fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
                                        ReDim ViewableBytes(FileLength)
                                        'Dim BytFileHolder() As Byte = New Byte(Chunk - 1) {}
                                        numBytesToRead = FileLength
                                        numBytesRead = 0
                                        fsSource.Seek(0, SeekOrigin.Begin)       'Getting Chunk Size FileHolder
                                        While (numBytesToRead > 0)

                                            Dim nx As Integer = fsSource.Read(ViewableBytes, numBytesRead, numBytesToRead)
                                            If (nx = 0) Then
                                                Exit While
                                            End If
                                            numBytesRead = (numBytesRead + nx)
                                            numBytesToRead = (numBytesToRead - nx)
                                        End While
                                    End Using
                                    kjR = kj
                                    XPBackground.ShowPicture()
                            End Select
                        End If
                End Select
                If ComingBack = True Then
                    kj = FileNumber
                End If
NextKJSecondTime:
                If kj < FileNumber Then
                    'SpecialFilltheList()
                End If
            Next kj
            'If didSingLoops = True Then
            '    PrivatePassword = ""
            '    Me.GreenLight()
            '    InternalKey = ""
            '    For ic = 0 To 31
            '        BDsingletonBytes(ic) = 0
            '    Next
            '    didSingLoops = False
            'End If
            ProgressBar2.Value = 0
            ProgressBar2.Visible = False

            If Looper = True And ComingBack = False Then
                GoTo starttheLoop
            ElseIf Looper = True And ComingBack = True Then
                Looper = False

            End If
            If GoViewer = True And ViewSequentially = False Then

                GoViewer = False
                'TurnOnButtons()
                FillTheList()
                Exit Sub
            ElseIf GoViewer = True And ViewSequentially = True Then
                XPBackground.Close()
                GoViewer = False
                'TurnOnButtons()
                FillTheList()
                Exit Sub
            End If
            '            If (Gotcha = 0 And DecryptFromPrivateKey = False) And Cmd = "DECRYPT" Then
            '                TurnOnButtons()
            '                FillTheList()
            '                If RunDouble Then
            '                    Gotcha = 0
            '                End If
            '                Exit Sub
            '               End If
            '            If RunDouble = 0 Then
            '                If Gotcha Or DecryptFromPrivateKey Then
            '                    If AutoEncryptionSequence = 0 Then
            '                        'frmAWAY32.Refresh
            '                    End If
            '                    'If Cmd$ = "ENCRYPT" Then
            '                    '  frmMsgBox.Show
            '                    ' frmMsgBox.Refresh
            '                    ' End If

            '                End If

            ' FilesExtension(kj) = UUnn


            '                FileDone% = 0
            'Dim FileQuantity As Integer, rb As Integer
            'If PubDirFiles > 0 Then
            '    FileQuantity = PubDirFiles
            'Else
            '    FileQuantity = FileNumber
            'End If
            'For rb = 1 To FileQuantity
            '    '                    If PubDirFiles Then
            '    '               Open PublicDirectory$ & "\" & PubDirFilesName(rb) For Binary As #4
            '    '                        FileLength = LOF(4)
            '    '                  Get #4, FileLength - 5, UUnn$
            '    '               Close #4
            '    '                    Else

            '    '               Open ActualPath$ & BackSlash & FileNamesrb) For Binary As #5
            '    '                        FileLength = FileLen(ActualPath$ & BackSlash & FileNames(rb))
            '    '                        UUnn$ = Space(3)
            '    '                        If FileLength > 20 Then
            '    '                     Get #5, FileLength - 5, UUnn$
            '    '                        End If
            '    '                    End If
            '    '            Close #4
            '    '         Close #5
            '    '                    'RealName = FileNames(rb)
            '    ''                           '        FilesExtension(rb) = UUnn
            '    Select Case UUnn
            '        Case "dd&"
            '            '        Dim NameFile As String
            '            '        NameFile = RealName(rb)

            '            If GetNewSessionInfo = True Then
            '                DecryptPublicEncryptedFile(combItem, strippedPEtemp, rb)
            '            End If
            '        Case "jP$", "jP*", "jj$", "jj*", "rr&", "##&", "ff&", "%%&", "ss&", "vv&"

            '            GenerateInternalKey(PrivatePassword$)
            '            PasswordInRamTimer.Enabled = True
            '            'frmVisiblePassword.DoTheThing
            '            'frmProcessFiles.flbFilesinProcess.Path = Pat$
            '            'frmProcessFiles.flbFilesinProcess.Enabled = True
            '            'frmProcessFiles.txtProcessDirectory.Text = Pat$
            '            FileToBeEncrypted(rb) = True
            '            Encrypt(Cmd$, combItem, RealName, rb)

            '        Case Else
            '            If Cmd = "ENCRYPT" Then
            '                FileToBeEncrypted(rb) = True

            '                Encrypt(Cmd$, combItem, RealName, rb)
            '            End If
            '    End Select
            'If ComingBack Then
            ComingBack = False
            'Cursor.Current = System.Windows.Forms.Cursors.Default
            'Exit For
            ' End If
            'Next rb
            FillTheList()
            '                Unload frmProcessFiles
            '                TurnOnButtons()
            '                FillTheList()
            '                ComingBack = 1

            '                'DumpTheForm

            '            End If

            '        End If
            '        ComingBack = 0
            '        Exit Sub
            'wrongdisk:
            '        msg = "Error # " & Str(Err.Number) & " was generated by " _
            '        & Err.Source & Chr$(13) & Err.Description
            '        MsgBox(msg, , "Error", Err.HelpFile, Err.HelpContextt)
            '        frmSignal.Hide()
            '        TurnOnButtons()
            '        FillTheList()
            '        ComingBack = 1
            '        DumpTheForm()
        Catch ex As Exception
            MessageBox.Show("You can not process files in Away RJN Cryptography" & vbCrLf & "while Windows Explorer or any other App (Photoshop, etc.)" & vbCrLf & "is accessing your Special Folder," & combItem & vbCrLf & "Close those Apps and continue", "Can't Access File.", MessageBoxButtons.OK)
            'If File.Exists(TentFile) Then
            '    Kill(TentFile)
            'End If
            ComingBack = True
            Exit Sub
        End Try
    End Sub
    Public Sub GetSendersEmailSessionKey(kj As Integer)
        Dim byChar As Integer = 0
        Dim strBlnkg As String = ""
        Dim SesIDByt() As Byte = New Byte(9) {}
        pathSource = AnalysedFile(kj, 0)
        'Using fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)

        Dim srSource As New StreamReader(pathSource, System.Text.UTF8Encoding.UTF8)
        Dim Blnkg() As Byte = New Byte(8) {}

        Do Until byChar = 9

            Blnkg(byChar) = CByte(srSource.Read)
            byChar += 1
        Loop

        srSource.Close()

        For n = 0 To 8
            strBlnkg = strBlnkg & Microsoft.VisualBasic.ChrW(Blnkg(n))
        Next n


        If strBlnkg <> BlankingSTR Then
            Stop
        End If
        Dim fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
        Dim amail() As Byte = New Byte(2) {}
        numBytesToRead = 3
        numBytesRead = 0
        fsSource.Seek(12, SeekOrigin.Begin)       'Getting lehgth of email
        While (numBytesToRead > 0)
            Dim nx As Integer = fsSource.Read(amail, numBytesRead, numBytesToRead)
            If (nx = 0) Then
                Exit While
            End If
            numBytesRead = (numBytesRead + nx)
            numBytesToRead = (numBytesToRead - nx)
        End While
        LenmaiL = CStr(Val(System.Text.Encoding.ASCII.GetString(amail)))
        'fsSource.Close()
        Dim SendEmaSTR As String
        'Dim fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
        Dim SenEmaByt() As Byte = New Byte(CInt(LenmaiL) - 1) {}   'Length of byte
        numBytesToRead = CInt(LenmaiL)
        numBytesRead = 0
        fsSource.Seek(15, SeekOrigin.Begin)       'Getting Senders email
        While (numBytesToRead > 0)
            Dim nx As Integer = fsSource.Read(SenEmaByt, numBytesRead, numBytesToRead)
            If (nx = 0) Then
                Exit While
            End If
            numBytesRead = (numBytesRead + nx)
            numBytesToRead = (numBytesToRead - nx)
        End While
        'bytTem = cnvBytesFromB64Str(tem)
        SendEmaSTR = System.Text.Encoding.ASCII.GetString(SenEmaByt)
        SendersEMail = Cnv.StringFromBase64(SendEmaSTR)
        ' fsSource.Close()
        'Dim fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)
        Dim SesNumByt() As Byte = New Byte(3) {}
        numBytesToRead = 4
        numBytesRead = 0
        fsSource.Seek(15 + CInt(LenmaiL), SeekOrigin.Begin)       'Getting Session Number
        While (numBytesToRead > 0)
            Dim nx As Integer = fsSource.Read(SesNumByt, numBytesRead, numBytesToRead)
            If (nx = 0) Then
                Exit While
            End If
            numBytesRead = (numBytesRead + nx)
            numBytesToRead = (numBytesToRead - nx)
        End While

        strSessionNumber = System.Text.Encoding.ASCII.GetString(SesNumByt)
        If PreviousSessionNumber = "" Then
            PreviousSessionNumber = strSessionNumber
            GetNewSessionInfo = True
        ElseIf PreviousSessionNumber = strSessionNumber Then
            GetNewSessionInfo = True
        Else
            GetNewSessionInfo = False
        End If


        '
        'Dim fsSource As FileStream = New FileStream(pathSource, FileMode.Open, FileAccess.Read)

        numBytesToRead = 10
        numBytesRead = 0
        fsSource.Seek(15 + CInt(LenmaiL) + 4, SeekOrigin.Begin)       'Getting Session ID
        While (numBytesToRead > 0)
            Dim nx As Integer = fsSource.Read(SesIDByt, numBytesRead, numBytesToRead)
            If (nx = 0) Then
                Exit While
            End If
            numBytesRead = (numBytesRead + nx)
            numBytesToRead = (numBytesToRead - nx)
        End While
        '
        fileSessionID = System.Text.Encoding.ASCII.GetString(SesIDByt)

        fsSource.Close()
    End Sub

    '---------------------------------------------------------------------------------------
    ' Procedure : TypeofKey
    ' DateTime  : 3/2/2012 12:36
    ' Author    : Rich
    ' Purpose   :
    '---------------------------------------------------------------------------------------
    '
    Public Sub TypeofKey()
        Dim DoneSomething As Boolean
        'On Error GoTo TypeofKey_Error
        DoneSomething = False
        'frmAWAY32.ListView1.HideSelection = False
        If DarbyCode = True Then
            PrivatePassword$ = ""
        End If
        If (Cmd$ = "ENCRYPT" Or GoViewer) Then
            If Me.RadioButton1.Checked Then    'TYPE INPUT
                If DarbyCode = True Then
                    PictPasswdNotice.Show()
                    PictPasswdNotice.Refresh()
                    Dim start, finish As Double
                    start = Microsoft.VisualBasic.DateAndTime.Timer
                    ' Set end time for 5-second duration.
                    finish = start + 3.0
                    Do While Microsoft.VisualBasic.DateAndTime.Timer < finish
                        ' Do other processing while waiting for 3 seconds to elapse. 
                    Loop
                    PictPasswdNotice.Close()
                    SHOWPICTURE()               '================================

                Else
                    With dlgPrivatePasswordVisible
                        '        .TextBox1.Visible = True
                        '        .Label1.Visible = True
                        .ShowDialog()
                    End With
                End If
            ElseIf Me.RadioButton2.Checked Then                  'PICTURE INPUT

                SHOWPICTURE()               '================================

            End If
        Else
            If Cmd = "DECRYPT" And PublicEncrypted = True Then
                dlgVisiblePasswordSessionKey.ShowDialog()
            ElseIf Cmd = "DECRYPT" Then
                If DarbyCode = False Then    'TYPE INPUT

                    With dlgPrivatePasswordVisible
                        .ShowDialog()
                    End With
                    'End If
                ElseIf DarbyCode = True Then                  'PICTURE INPUT
                    If Not Me.RadioButton2.Checked Then
                        PictPasswdNotice.Show()
                        PictPasswdNotice.Refresh()
                        Dim start, finish As Double
                        start = Microsoft.VisualBasic.DateAndTime.Timer
                        ' Set end time for 5-second duration.
                        finish = start + 3.0
                        Do While Microsoft.VisualBasic.DateAndTime.Timer < finish
                            ' Do other processing while waiting for 3 seconds to elapse. 
                        Loop
                        PictPasswdNotice.Close()
                        With dlgPrivatePasswordVisible
                            .ShowDialog()
                        End With
                    Else
                        SHOWPICTURE()               '================================
                    End If


                End If
            End If
        End If
    End Sub


    Public Sub SendEMail(AttachFiles%, SendTo$, MsgSubject$, CiphContent$)
        Dim temp As String = "", i As Integer, Att As Integer, FoundFiles As Integer, NewContent As String = ""
        Dim whom$ = ""
        Dim TBird$ = ""
        Dim body$ = ""
        Dim Separation As String = ""
        Dim attach As String = ""
        If StandardNoteEncrypting = False Then
            If SendCert = True Then
                attach = strCertName
                body = "Hello " & SendTo & "," & SmallBreak$ & "Since this is the first time for communication with " _
                    & RecipsEMail & ", the X509 Certificate," & SmallBreak$ &
                    "which will be used to verify that future Public Key Encrypted files are from " _
                    & RecipsEMail &
                    ", is attached." & SmallBreak$ & "If you have a 64 bit system, save it to your 'Program Files (x86)\BMC Engineering\Certs' folder. " & SmallBreak$ & "Or, if you have a 32 bit system save it to your 'Program Files\BMC Engineering\Certs' folder. " & Paragraph &
                "There is nothing further that needs to be done for verification. The Public Key will be sent to you in another email." & SmallBreak & "If you get a Public Key from someone who purports to be " & RecipsEMail & "," & SmallBreak & "the x509 Certificate will see that and Away RJN Cryptography will inform you."

            Else

                CiphContent = "***" & CiphContent & "***"
                i = Len(CiphContent)
                NewContent = NewContent$ & Strings.Mid(CiphContent, 1, 59) & SmallBreak
                i = 60
                NewContent$ = NewContent$ & Strings.Mid$(CiphContent, i, 59) & SmallBreak$
                i = 119
                NewContent$ = NewContent$ & Strings.Mid$(CiphContent, i, 59) & SmallBreak$
                i = 178
                NewContent$ = NewContent$ & Strings.Mid$(CiphContent, i, 59) & SmallBreak$
                i = 237
                NewContent$ = NewContent$ & Strings.Mid$(CiphContent, i, 59) & SmallBreak$
                i = 296
                NewContent$ = NewContent$ & Strings.Mid$(CiphContent, i, 59) & SmallBreak$
                i = 355
                NewContent$ = NewContent$ & Strings.Mid$(CiphContent, i, 59) & SmallBreak$
                i = 414
                If Len(CiphContent) > 414 Then
                    NewContent$ = NewContent$ & Strings.Mid$(CiphContent, i, 59) & SmallBreak$
                    i = 473
                End If
                NewContent$ = NewContent$ & Strings.Mid$(CiphContent, i)

                Dim SignatureizedFile$(5)
                If AttachFiles% > 0 Then

                    FoundFiles% = 1
                    SignatureizedFile$(FoundFiles%) = Dir(PublicDirectory$ & "*.*")
                    Do
                        FoundFiles% = FoundFiles% + 1
                        temp$ = Dir()
                        If temp$ <> "" Then
                            SignatureizedFile$(FoundFiles%) = temp$

                        End If
                    Loop Until temp$ = ""
                End If

                If MsgSubject.StartsWith("Recipient's Public Key") = True Then
                    '.MsgSubject =
                    MsgSubject = MsgSubject & "-- of  " & PubKeyDate
                    FileOpen(1, OrigDirectory$ & "Keys\" & SendersEMail$ & ".txt", OpenMode.Output)
                    PrintLine(1, PubKeyDate)
                    FileClose(1)

                Else
                    MsgSubject$ = MsgSubject$ & "----Session Number " & Trim(strSessionNumber)               '"Digital Signature"
                End If
                '.MsgIndex = -1
                '.MsgNoteText = CiphContent
                Dim CombinedPublicNote As String
                If doingPublicKey = False Then
                    PreviousPublicKeyExpired = ""
                    CombinedPublicNote = ""
                Else
                    CombinedPublicNote = "This is your combined email and Public Key. It will separate at the right time."
                End If
                body$ = MsgSubject$ & Paragraph$ & PreviousPublicKeyExpired & CombinedPublicNote & Paragraph _
                    & "-----------Begin Away RJN Cryptography Key " & "----------" & Paragraph$ _
                & NewContent$ & Paragraph$ _
                & "------------End Away RJN Cryptography Key --------------" & Paragraph$ _
                & "Copy and Paste above key including asterisks into the Away RJN Cryptography Form when it asks you. " & SmallBreak$ _
                & "Important: ...Include ALL characters. DO NOT include any explanatory text."

                If MsgSubject.StartsWith("Recipient's Public Key") = False Then
                    body$ = "--RECIPIENT -->" & SmallBreak$ & "From " & SendersEMail & Paragraph$ & body$ & Paragraph$ & Paragraph$ & "Save Attached File(s)" _
                    & "to one of your Special Folders and Decrypt as usual, and you will need the above Session Key."
                End If

                If AttachFiles% > 0 Then
                    Att = 0
                    attach$ = ""
                    '.AttachmentType = ATTACHTYPE_DATA
                    '.AttachmentPosition = Len(.MsgNoteText)
                    Dim beginningQuote$
                    For i = 0 To FoundFiles% - 2
                        '.AttachmentIndex = Att
                        Att = Att + 1
                        If i = 0 Then
                            beginningQuote$ = "'"
                        Else
                            beginningQuote$ = ""
                        End If
                        If i = FoundFiles% - 2 Then
                            Separation$ = "'"
                        ElseIf i < FoundFiles% - 2 Then
                            Separation$ = ","
                        End If
                        attach$ = attach$ & beginningQuote$ & PublicDirectory$ & "\" & SignatureizedFile$(Att) & Separation$
                        '.AttachmentName = SignatureizedFile$(Att)
                        '.AttachmentPathName = PublicDirectory$ & "\" & SignatureizedFile$(Att)

                    Next i
                    'Open "body.txt" For Output As #8
                    '  Print #8, attach$
                    'Close #8
                End If

            End If
            whom$ = SendTo$              'RecipsEMail$
        Else       'This is sending encrypred note attached

            body = "Private Password Encrypted Message attached." & SmallBreak & "If you have a 64 bit system, save it to your 'Program Files (x86) > BMC Engineering > Encrypted_Inbox' folder. " & SmallBreak$ & "Or, if you have a 32 bit system save it to your 'Program Files > BMC Engineering > Encrypted_Inbox' folder. " & SmallBreak & "Or, the easiest is to save it to 'This PC > Documents' folder." & Paragraph & "To decipher this message, you must use the " & SmallBreak & "same Password that was used to encrypt it."
            MsgSubject = "Private Enc Message."
            attach$ = OrigDirectory & "Encrypted_Sent_Email\" & CorrectedFileName
        End If
        Dim RetValtbrd As Integer
        TBird$ = TBirdDrive$ & "\thunderbird.exe -compose " & Chr(34) + "to='" & whom$ & "',format='1',subject='" & MsgSubject$ & "',body='" & body$ & "',attachment=" & attach$
        'Dim Argumen As String = " -compose " & Chr(34) + "to='" & whom$ & "',format='1',subject='" & MsgSubject$ & "',body='" & body$ & "',attachment=" & attach$

        'Dim startInfo As New ProcessStartInfo(TBirdDrive & "\thunderbird.exe")
        'Process.Start(startInfo)
        'startInfo.Arguments = Argumen
        'Process.Start(startInfo)

        RetValtbrd = Shell(TBird, AppWinStyle.NormalFocus)


        'HERE IS THE PROPOERLY FORMATTED COMMAND LINE
        'TBird$ = OrigDrive$ & "Program Files\Mozilla Thunderbird\thunderbird.exe -compose " & Chr(34) + "to='" & whom$ & "',format='1',subject='" & MsgSubject$ & "',body='" & body$ & "',attachment=" & attach$

        'All from http://kb.mozillazine.org/Command_line_arguments_-_Thunderbird
        'End With

        Exit Sub




    End Sub



    '---------------------------------------------------------------------------------------
    ' Procedure : Form_KeyDown
    ' DateTime  : 4/28/2012 14:58
    ' Author    : Rich
    ' Purpose   : Utilize Shift + SpaceBar   OR Contol + Spacebar  OR Alt + SpaceBar
    '---------------------------------------------------------------------------------------
    '
    Private Sub ControlScreen_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        Dim qni As Integer, punched As Integer, positi As Long, itmX As ListViewItem, cunt As Integer
        Dim PunchFile() As String
        On Error GoTo Form_KeyDown_Error
        If (e.Shift And e.KeyCode = Keys.Space) Or (e.Alt And e.KeyCode = Keys.Space) Or (e.Control And e.KeyCode = Keys.Space) Then
            cunt = 0
            punched = 1
            If WeGotOne > 0 Then
                ReDim PunchFile(WeGotOne)
                For Each itmX In ListView1.Items
                    FileNames$(cunt + 1) = itmX.Text
                    qni = InStr(FileNames$(cunt + 1), ".")
                    If UCase$(Mid$(FileNames$(cunt + 1), qni + 1, 3)) = "AAA" Then
                        PunchFile(punched%) = FileNames$(cunt + 1)
                        punched% = punched% + 1
                    End If
                    cunt = cunt + 1
                Next


                punched% = 2
                For qni = 1 To WeGotOne
                    Dim st6 As Stream = File.Open(combItem & PunchFile(qni), FileMode.Create, FileAccess.Write)
                    Dim sw6 As New BinaryWriter(st6)

                    sw6.Write(Slug(punched))
                    sw6.Close()
                    '  st6.Close()
                    'Open Pat$ & "\" & PunchFile(qni) For Binary As #1
                    '   Put #1, 1, slug$(punched)
                    '  Close()
                    punched = punched + 1
                    If punched = 99 Then
                        punched = 2
                    End If
                Next
            End If
        End If
        If e.KeyCode = Keys.Delete Then
            PermDeleteFiles()
        End If
        On Error GoTo 0
        Exit Sub

Form_KeyDown_Error:

        MsgBox("Error " & Err.Number & " (" & Err.Description & ") in procedure Form_KeyDown of Form frmAWAY32")

    End Sub


    Public Sub DoDoneButton()
        HighLightedFile = 0

        cunt = 0

        'GroupBox6.Enabled = False
        'GroupBox6.Visible = False
        If ComingBack Then
            ComingBack = False
            GoTo fileout
        End If




        If InStr(combItem, "None Yet") = 0 And FileNumber > 0 Then
            ReDim FileToBeEncrypted(FileNumber)
            ReDim PublicFileNames(FileNumber)
            ' ReDim FilesExtension(FileNumber)
            FileNumber = 0

            '*********************PICK AND CHOOSE **********
            Dim HighlightedGroup As ListView.SelectedListViewItemCollection = Me.ListView1.SelectedItems
            Dim item As ListViewItem
            For Each item In HighlightedGroup

                HighLightedFile = HighLightedFile + 1
                FileToBeEncrypted(cunt) = True
                PublicFileNames(HighLightedFile) = item.Text

                FileNumber += 1

                cunt += 1
            Next




            If HighLightedFile > 5 Then
                MessageBox.Show("You need to select 5 or less files at a time to be Public Key Encrypted.", "Too Many Files", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

                'GroupBox6.Enabled = True
                'GroupBox6.Visible = True
                'Button10.Visible = False
                'Label12.Visible = False
                'Label13.Visible = False
                'PictureBox1.Visible = False
                'PictureBox2.Visible = False
                Exit Sub

            ElseIf HighLightedFile = 0 Then
                'groupbox6.Enabled = True
                Me.Label11.Visible = False
                Me.DoneButton14.Visible = False
                ComingBack = True
                Exit Sub
            End If
            'temp$ = GetSetting("AwayRJN", "startup", "frmDigiSign.chkDontShow")
            'If temp$ = "0" Or temp$ = "" Then
            'frmDigiSign.Show vbModal
        End If



        If ComingBack Then
            ComingBack = False
            Cursor.Current = System.Windows.Forms.Cursors.Default
            GoTo fileout
        End If


        'FindOutAboutEMailHtml

        Cursor.Current = System.Windows.Forms.Cursors.WaitCursor


        'UUnn$ = "   "

        ReDim Preserve PublicFileNames(5)
        For kjm = 1 To HighLightedFile
            FileCopy(combItem & PublicFileNames(kjm), PublicDirectory & PublicFileNames(kjm))
        Next kjm
        'UUnn$ = Space(3)
        ' Some keys we prepared earlier (see CreateTestKeys)

        'FIRST WE CREAT SESSION KEY
        strSessionNumber = Space(4)

        PartialDoneButton()
fileout:
        Cursor.Current = System.Windows.Forms.Cursors.Default
        EncryptingFileWithSignature = False
        DoingPublic = False
        ''temp$ = PrivatePassword$
        'TurnOnButtons()
        'flbList1.Refresh()
        'ListView1.Refresh()
        'GroupBox6.Enabled = False
        'GroupBox6.Visible = False
        'Button10.Visible = True
        Label12.Visible = True
        Label13.Visible = True
        PictureBox1.Visible = True
        PictureBox2.Visible = True
        'TabControl1.Enabled = True
        'TabControl1.TabPages.RemoveAt(4)
        'TabControl1.SelectedTab = TabControl1.TabPages(0)
        ComingBack = False
        Me.FillTheList()
        Exit Sub
    End Sub
    Private Sub DoneButton14_Click(sender As Object, e As EventArgs) Handles DoneButton14.Click
        'DONE now for Encrypting and Sending Publically encrypted files
        DoDoneButton()
        Me.Label11.Visible = False
        Me.DoneButton14.Visible = False
    End Sub
    Public Sub PartialDoneButton()
        Try
            Dim sr7 As New StreamReader(OrigDirectory & "Keys\" & "SessionNumber.txt")
            strSessionNumber = sr7.ReadLine
            sr7.Close()
            intSessionNumber = CInt(Val(strSessionNumber))
        Catch
        End Try
        DoingPublic = True
        EncryptingFileWithSignature = True
        intSessionNumber = intSessionNumber + 1
        If intSessionNumber = 1000 Then
            intSessionNumber = 1
        End If
        strSessionNumber = NumPad$(intSessionNumber)
        Dim sw7 As New StreamWriter(OrigDirectory & "Keys\" & "SessionNumber.txt")
        sw7.WriteLine(strSessionNumber)
        sw7.Close()
        ' (1) In this first part of the example, we are the sending party
        ' We have the recipient's public key which is generally available

        ' Read in the public key as base64 string
        'sPublicKey64 = RsaReadPublicKey(sPublicKeyFile)

        Dim strInternalKey As String
        '***************First remove the PubKeyDate
        LeftPart = Strings.Left(RecipsPublicKey, 18)
        PubDateKey = Strings.Mid(RecipsPublicKey, 19, 32)
        RightPart = Strings.Mid(RecipsPublicKey, 51)
        RecipsPublicKey = LeftPart & RightPart
        Dim nRet As Integer = Len(RecipsPublicKey)
        Dim Frontstr As String = "<RsaKeyValue><Modulus>"
        Dim Backstr As String = "</Modulus><Exponent>BQ==</Exponent></RsaKeyValue>"
        nRet = Len(RecipsPublicKey)              ' = 172
        RecipsPublicKey = Frontstr & RecipsPublicKey & Backstr
        strInternalKey = Rsa.FromXMLString(RecipsPublicKey, False)
        '****************************************DID NOT REMOVE HASH FROM END ***********************************************
        'nLen = Rsa_FromXMLString("", 0, RecipsPublicKey, 0)
        If strInternalKey.Length = 0 Then
            MsgBox("Error: ")
            Stop
        End If
        'strInternalKey = String(nLen, " ")
        'nRet = Rsa_FromXMLString(strInternalKey, Len(strInternalKey), RecipsPublicKey, 0)
        'strInternalKey = Strings.Left(strInternalKey, CInt(nLen))

        'Debug.Print "INTKEY=" & strInternalKey

        nRet = Rsa.CheckKey(strInternalKey)
        'Debug.Print "Rsa_CheckKey returns " & nRet

        sPublicKey64 = strInternalKey
        i = Len(sPublicKey64)

        ' Check its length - it should be 128 bytes (1024 bits)
        nBlockLen = Rsa.KeyBytes(sPublicKey64)       'also shrinks from base64
        If nBlockLen < 128 Then
            MessageBox.Show("You did not get the whole Public Key. Try again.", "Incomplete Public Key", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
            GoTo fileout
        End If
        ''.Print "Public key is " & nBlockLen & " bytes long (" & nBlockLen * 8 & " bits)"

        ' We could, as an alternative, hardcode in the base64 value to start with
        ' (should we be sending lots of messages to this person, or maybe we keep it in a lookup table)
        ''.Print "PUBK= " & sPublicKey64

        ' Get the text we want to encrypt (max (128-10)= 118 bytes long)
        'strPlain = strRandomPassword
        ''.Print "DATA= " & strPlain

        ' Convert to bytes
        ' abPlain = StrConv(strPlain, VbStrConv.None)

        ' Show in hex
        ''.Print "DATA= " & cnvHexStrFromBytes(abPlain)
        ' Check exact length in bytes
        '****************************MY random password string ****
        'Dim FirstSegment As Boolean, CTRNonce As String, i%
        'FirstSegment = False
        'ChristieNumber = CStr((DateDiff("s", "5/10/1968", Now)) - 1000000000)
        'NonceKey = Left$(PrivatePassword, 14)
        'MakeCTRNonce(CTRNonce, ChristieNumber, FirstSegment)
        'RandShuffle(CTRNonce)
        'MakeRandomTable(RandomTable%(), 32, "GenIntLey")
        'strRandomPassword = ""
        'For i = 1 To 32
        '    strRandomPassword = strRandomPassword & CStr(bytRandomPassword(i))
        'Next
        ''strRandomPassword = "12345678901234567890123456789012"       'StrConv(bytRandomPassword, vbUnicode)

        Dim MyRand As New Random(CInt(DateDiff("s", "5/10/1968", Now)))
        Dim bytRandomPassword(31) As Byte '= Rng.Bytes(32)
        nBytes = UBound(bytRandomPassword) - LBound(bytRandomPassword) + 1
        strRandomPassword = ""
        For p = 1 To 32
            strRandomPassword = strRandomPassword & MyRand.Next(0, 9)
        Next p
        For j = 0 To 31
            bytRandomPassword(j) = Convert.ToByte(CChar(Strings.Mid(strRandomPassword, j + 1, 1)))
        Next
        ' Construct encryption block of exactly 64 bytes
        ReDim abBlock(nBlockLen - 1)

        ' Required encryption block (Ref: EME-PKCS1-V1_5-ENCODE from PKCS#1)
        '|<------------------(128 bytes)---------------->|
        '+--+-------+--+--------------------------------+
        '|02|PADDING|00|      DATA TO ENCRYPT           |
        '+--+-------+--+--------------------------------+

        ' First byte is always 02
        abBlock(0) = 2
        ' Then pad with at least 8 non-zero random bytes
        nPad = nBlockLen - 2 - nBytes

        If nPad < 8 Then
            MsgBox("Data is too long/ key is too short")
            'Exit Function
        End If

        ' The random bytes just need to be different from before
        ' so we don't need to use the secure generator

        Randomize()
        For q = 1 To nPad
            Do
                abBlock(q) = CByte(CLng(Int(Rnd() * 256)) And &HFF)
            Loop While abBlock(q) = 0
        Next

        ' Separating NUL byte
        abBlock(nPad + 1) = 0

        ' Then the data to be encrypted
        For r = 0 To nBytes - 1
            abBlock(r + nPad + 2) = bytRandomPassword(r)
        Next r

        ' Show the block in hex
        ''.Print "BLCK= " & cnvHexStrFromBytes(abBlock)

        ' Given the encrypted data in abBlock,
        ' we use the Rsa function to encrypt the input block using the public key
        'ENCRYPTS THE RANDOM PASSWORD into the abBlock(0)
        'abData = Rsa.RawPublic(abData, sbPublicKey.ToString())
        Dim abData() As Byte
        abData = Rsa.RawPublic(abBlock, CStr(sPublicKey64))
        ''.Print "Rsa_RawPublic returns " & lngRet

        ' Display our results in hex format
        ''.Print "ENCR=" & cnvHexStrFromBytes(abBlock)

        ' Now we send the 128 bytes of ciphertext in abBlock to our recipient
        ' (perhaps we encode them first in hex or base64, it's up to you)
        ' Note that the ciphertext will be completely different each time
        ' even for the same text with the same key, because of the random padding
        strSessionKey = Cnv.ToBase64(abData)
        Dim dlen As Integer = Len(strSessionKey)    'should be
        SessionID = Strings.Right(strSessionKey, 10)
        '***********************************INSERT PUBKEY DATE HERE
        LeftPart = Strings.Left(strSessionKey, 18)
        RightPart = Strings.Mid(strSessionKey, 19)
        strSessionKey = LeftPart & PubDateKey & RightPart
        dlen = Len(strSessionKey)    'should be
        ' ReDim SignatureizedFile$(HighLightedFile)
        ReDim AnalysedFile(HighLightedFile, 4)
        For kjl = 1 To HighLightedFile
            Try
                PublicDirectoryFileNmae = PublicDirectory & PublicFileNames$(kjl)
                ThisFile = Path.GetFileName(PublicDirectoryFileNmae)
                FileLength = CInt(FileLen(PublicDirectoryFileNmae))
                Dim fs As New FileStream(PublicDirectoryFileNmae, FileMode.Open, FileAccess.Read)
                Dim wholeEnd As String = " "
                Dim nextByte As Integer
                fs.Seek(FileLength - 6, SeekOrigin.Begin)
                nextByte = fs.ReadByte()
                While (nextByte > 0)
                    wholeEnd = wholeEnd & Convert.ToChar(nextByte)
                    nextByte = fs.ReadByte()
                End While
                wholeEnd = Trim(wholeEnd)
                fs.Close()
                If FileLength > 25 Then
                    UUnn = Strings.Left(wholeEnd, 3)
                    SavedLengthFileName = Strings.Right(wholeEnd, 3)
                Else
                    GoTo fileout
                End If

                AnalysedFile(kjl, 0) = PublicDirectoryFileNmae
                AnalysedFile(kjl, 1) = SavedLengthFileName
                AnalysedFile(kjl, 2) = UUnn

                Select Case UUnn$
                    Case "jP$", "jP*", "jj$", "jj*", "rr&", "##&", "ff&", "%%&", "ss&", "vv&", "jPk", "jjk", "tPk", "tjk", "jPL", "jjL"
                        DoingPublic = True
                        'ConvertingFromPrivateEncrypttoPublic = True
                        'frmAWAY32.Refresh()

                        PasswordInRamTimer.Enabled = True
                        Cmd$ = "DECRYPT"    'decrypt first
                        If PrivatePassword = "" Then
                            MessageBox.Show("One or more of the files you selected is Encrypted with your Private Password. It has been copied to RAM to be processed for Public Key Encryption. Your Privately Encrypted file remains untouched.", "Decrypt Private File", MessageBoxButtons.OK)
                            dlgPrivatePasswordVisible.ShowDialog()
                        End If
                        GenerateInternalKey(PrivatePassword$)
                        AnalysedFile(kjl, 3) = "RegEncrypted"
                        Encrypt(Cmd$, PublicDirectory$, ThisFile, kjl)
                        AnalysedFile(kjl, 3) = "PlainText"
                        AnalysedFile(kjl, 4) = "0"
                        If ComingBack = True Then
                            ComingBack = False
                            Cursor.Current = System.Windows.Forms.Cursors.Default
                            Exit Sub
                        End If
                        'Rename(PublicDirectory$ & "\" & PublicFileNames$(kj), PublicDirectory$ & "\" & RealName$)
                        ThisFile = RealName$
                    Case "dd&"
                        Exit For
                        'Digital Signatureized!!!!already
                    Case "  &"
                        'OK to encrypt
                    Case Else
                        AnalysedFile(kjl, 3) = "PlainText"
                        ''OK to encrypt
                End Select
                ''DoingPublic% = 1

                Cmd$ = "ENCRYPT"          'encrypt inside export
                'frmAWAY32.Refresh()
                ' strRandomPassword = "12345678901234567890123456789012"
                GenerateInternalKey(strRandomPassword)

                Encrypt(Cmd$, PublicDirectory$, ThisFile, kjl)    'Encrypting the files being sent

                'with the random password
                ''SignatureizedFile$(kj) = PublicDirectory$ & PublicFileNames$(kj)

                ''ss& = Public Encrypted!!! but not used
                ''dd& = Public Key Encrypted!!!!
            Catch
            End Try
        Next kjl


        SendEMail(HighLightedFile, RecipsEMail, "Session Key and Public Enc File(s)", strSessionKey$)

FileOut:
        Cursor.Current = System.Windows.Forms.Cursors.Default
        EncryptingFileWithSignature = False
        DoingPublic = False
        ''temp$ = PrivatePassword$
        'TurnOnButtons()
        'flbList1.Refresh()
        'ListView1.Refresh()
        'GroupBox6.Enabled = False
        'GroupBox6.Visible = False
        'Button10.Visible = True
        Label12.Visible = True
        Label13.Visible = True
        PictureBox1.Visible = True
        PictureBox2.Visible = True
        'TabControl1.Enabled = True
        ComingBack = False
        Me.FillTheList()
        Exit Sub
    End Sub

    Function NamePad$(FleNme$)
        Dim N As String, nb As Integer
        N$ = Trim$(Str$(Val(Len(FleNme$))))
        nb% = CInt(Val(N))
        Select Case nb%
            Case Is < 10
                N$ = "00" + N$
            Case Is < 100
                N$ = "0" + N$
        End Select
        NamePad$ = N$
    End Function


    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        GreenLight()
    End Sub

    Private Function WaitCurser() As Windows.Forms.Cursor
        Throw New NotImplementedException
    End Function

    Private Sub CheckBox4_CheckedChanged(sender As Object, e As EventArgs) Handles AutoSequ.CheckedChanged
        If AutoSequ.Checked Then
            ViewSequentially = True
        Else
            ViewSequentially = False
        End If

        SaveSetting("winexpl", "startup", "chkSequence", Trim$(Str(AutoSequ.CheckState)))
    End Sub
    Private Sub CheckBox7_CheckedChanged(sender As Object, e As EventArgs) Handles RepeatSequence.CheckedChanged
        If RepeatSequence.Checked Then
            Looper = True
        Else
            Looper = False
        End If

        SaveSetting("winexpl", "startup", "chkRepeat", Trim$(Str(RepeatSequence.CheckState)))
    End Sub

    Private Sub Button15_Click(sender As Object, e As EventArgs)
        'cancel button
        'GroupBox6.Enabled = False
        'GroupBox6.Visible = False
        'Button10.Visible = True
        Label12.Visible = True
        Label13.Visible = True
        'TabControl1.Enabled = True
        'TabControl1.TabPages.RemoveAt(4)

        PictureBox2.Visible = True
        ComingBack = True
        FillTheList()
        Exit Sub
    End Sub

    '---------------------------------------------------------------------------------------
    ' Procedure : mnuPicturePassword_Click
    ' DateTime  : 6/22/2012 20:08
    ' Author    : Rich
    ' Purpose   :
    '---------------------------------------------------------------------------------------
    '
    Private Sub ChangePictureForPasswordInputToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ChangePictureForPasswordInputToolStripMenuItem.Click


        Dim strPics As String, strPicFile As String, locate As Integer, length As Integer
        On Error GoTo mnuPicturePassword_Click_Error


        'SHOW DEFAULT PIC FIRST

        strPics = "Pictures|*.jpg;*.gif"
        With OFD11
            '.CancelError = True

            .Filter = strPics
            '.FilterIndex = 2
            .ReadOnlyChecked = False
            '.Flags = cdlOFNHideReadOnly
            .InitialDirectory = OrigDirectory
            .FileName = "DefaultPicturePassword.jpg"
            ' .ShowOpen()
            .Title = "Select Your Own Picture for Password."
            .ShowReadOnly = False
            If .ShowDialog = Windows.Forms.DialogResult.OK Then
                strPicFile = .FileName
                SaveSetting("winexpl", "picture", "forPassword", strPicFile)
            End If
            '.OpenFile()
            If .FileName = "" Then Exit Sub
            'locate = InStrRev(.FileName, "\")
            '   length = Len(.FileName)

            'strpicFile = cdl1.FileName.Remove(locate, length - locate)
            'STORE STRPICFILE IN REGITRY


            ' imgPicture.Image1.Picture = LoadPicture(strPicFile)
            ' MsgBox.FileName, , "You Selected ..."
        End With
        On Error GoTo 0
        Exit Sub

mnuPicturePassword_Click_Error:
        'Exit Sub
        MsgBox("Error " & Err.Number & " (" & Err.Description & ") in procedure mnuPicturePassword_Click of Form frmAWAY32")
    End Sub


    Private Sub NumericUpDown1_ValueChanged(sender As Object, e As EventArgs) Handles NumericUpDown1.ValueChanged
        'If NumericUpDown1.Value Then
        If startingload = False Then
            Rate = CInt(NumericUpDown1.Value)
            'ViewingTimer.Interval = Rate * 1000
            'ViewingTimer.Enabled = False
            'End If
            SaveSetting("winexpl", "startup", "txtRate", Trim$(CStr(Rate)))
        End If
    End Sub

    'Private Sub ViewingTimer_Tick(sender As Object, e As EventArgs) Handles ViewingTimer.Tick
    '    Pix = Pix + 1
    '    If WrongType Then
    '        If WrongType >= HighLightedFile Then
    '            ViewingTimer.Enabled = False
    '            Unload frmBackground
    '            Unload frmImage

    '            Unload frmCantShow
    '            Unload frmHiddenPassword
    '            Unload frmVisiblePassword
    '            TurnOnButtons()
    '            Exit Sub
    '        End If
    '        Unload frmCantShow
    '        'Exit Sub
    '    End If
    '    If Pix >= HighLightedFile + 1 Then
    '        If Looper Then
    '            Pix = 1
    '        Else
    '            ViewingTimer.Enabled = False
    '            Unload frmBackground
    '            Unload frmImage
    '            Unload XPBackground

    '            Unload frmHiddenPassword
    '            Unload frmVisiblePassword
    '            TurnOnButtons()
    '            Exit Sub
    '        End If
    '    End If

    '    HandlePicture()
    'End Sub

    Private Sub CheckBox6_CheckedChanged(sender As Object, e As EventArgs) Handles RandomView.CheckedChanged
        If RandomView.Checked Then
            RandomViewing = True
        Else
            RandomViewing = False
        End If

        SaveSetting("winexpl", "startup", "chkRandomView", Str(RandomView.Checked))
    End Sub



    Private Sub RadioButton4_CheckedChanged(sender As Object, e As EventArgs)
        'black background

    End Sub



    Private Sub Button20_Click_1(sender As Object, e As EventArgs)

    End Sub


    Private Sub Button21_Click(sender As Object, e As EventArgs)

    End Sub

    Private Sub ListView1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListView1.SelectedIndexChanged

    End Sub

    Private Sub Button2_Click_1(sender As Object, e As EventArgs)

    End Sub



    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        FillTheList()
    End Sub

    Private Sub LinkLabel4_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel4.LinkClicked
        'Public Encrypt File and Send
        Dim temp$


        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If


        If NeedAutoEncrytTimer Then
            chkAutoEncrypt.Checked = False
            'chkAutoDecryptch 0
        End If
        AutoEncryptTimer.Enabled = False


        'it has been determined if TBird is present

        'If NoTBird Then
        '    dlgEMailNotice.Show vbModal
        '    If ComingBack Then
        '        ComingBack = 0
        '        Exit Sub
        '    End If
        'End If
        On Error Resume Next
        Kill(PublicDirectory$ & "\*.*")
        On Error GoTo 0
        EncryptingFileWithSignature = True


        'Call RNG_Bytes(bytRandomPassword(0), 32, "", 0)
        ' Convert to a string
        '****************************MY random password string ****
        'Dim FirstSegment As Boolean, CTRNonce As String, i%
        'FirstSegment = False
        'ChristieNumber = CStr((DateDiff("s", "5/10/1968", Now)) - 1000000000)
        'NonceKey = Left$(PrivatePassword, 14)
        'MakeCTRNonce(CTRNonce, ChristieNumber, FirstSegment)
        'RandShuffle(CTRNonce)
        'MakeRandomTable(RandomTable%(), 32, "GenIntLey")
        'strRandomPassword = ""
        'For i = 1 To 32
        '    strRandomPassword = strRandomPassword & CStr(bytRandomPassword(i))
        'Next
        ''strRandomPassword = "12345678901234567890123456789012"       'StrConv(bytRandomPassword, vbUnicode)
        ''kj = Len(strRandomPassword)
        'ComingBack = False
        ''If chkPassword.Value Then
        ''  dlgSendHidEncdPubKDigS.Show vbModal
        ''Else
        ''   dlgSendVisiEncPubKDigS.Show vbModal
        ''End If
        'If frmAWAY32.Option2(0).Value = True Then    'TYPE INPUT
        '    If chkPassword.Value Then
        'With dlgSendHidEncdPubKDigS
        '    .Text1.Visible = True
        '    .Text2.Visible = True
        '    .Label1.Visible = True
        '    .Label2.Visible = True
        '    .Show()
        'End With
        '    Else
        With dlg2SendEncPubKeyFile
            ' .TextBox1.Visible = True
            ' .Label1.Visible = True
            .ShowDialog()
        End With
        '    End If
        'ElseIf frmAWAY32.Option2(1).Value = True Then                'PICTURE INPUT
        '    msg$ = "Show Picture for Password?"
        '    Style = vbOKCancel        ' Define buttons.
        '    Title$ = "Picture for Password Entry"
        '    Response = MsgBox(msg$, Style, Title$)
        '    If Response = vbCancel Then
        '        ComingBack = True
        '        Exit Sub
        '    End If
        '    SHOWPICTURE()               '================================
        '    dlgSendVisiEncPubKDigS.Show vbModal
        'ElseIf frmAWAY32.Option2(2).Value = True Then                'ONSCREEN INPUT
        '    frmMyKeyBoard.Show vbModal
        '    dlgSendVisiEncPubKDigS.Show vbModal
        'End If
        If ComingBack Then
            'TurnOnButtons()
            'flbList1.Refresh()
            ComingBack = False
            Exit Sub
        End If
        'groupbox6.Enabled = True
        'groupbox6.Visible = True
        'Button10.Visible = False
        'Label12.Visible = False
        'Label13.Visible = False
        'TabControl1.Enabled = False
        'PictureBox1.Visible = False
        'PictureBox2.Visible = False       'Done Box
        'TabControl1.TabPages.Insert(4, TabPage100)
        'TabControl1.SelectedTab = TabControl1.TabPages(4)

        'TurnOffButtons()
    End Sub

    Private Sub LinkLabel3_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel3.LinkClicked
        'DECRYPT

        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If


        If NeedAutoEncrytTimer Then
            'chkAutoEncrypt.Value = 0
            'chkAutomaticDecrypt.Value = 0
        End If

        Cmd = "DECRYPT"



        If ComingBack Then
            ComingBack = False
            Cursor.Current = System.Windows.Forms.Cursors.Default
            Exit Sub
        End If
        Cursor.Current = System.Windows.Forms.Cursors.WaitCursor
        'ListView1.UseWaitCursor = True
        Prepare()
        Cursor.Current = System.Windows.Forms.Cursors.Default
    End Sub

    Private Sub LinkLabel2_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked
        'Prepare Public Key
        '  Dim PublicKeyFile As String, PrivateKeyFile As String
        'Dim nChars As Integer, temp As String
        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If


        'If NeedAutoEncrytTimer Then
        '    chkAutoEncrypt.Value = 0
        '    chkAutomaticDecrypt.Value = 0
        'End If


        'Determine is TBird is present

        If NoTBird Then

            NoticeTBird.ShowDialog()
            If ComingBack = True Then
                ComingBack = False
                Cursor.Current = System.Windows.Forms.Cursors.Default
                Exit Sub
            End If
        End If
        '  DoingPublic% = 0
        '  TurnOffButtons()
        '  AutoEncryptTimer.Enabled = False
        '  ComingBack = 0
        If Me.RadioButton1.Checked Then    'TYPE INPUT

            With dlgPublicKeyVisiblePassword
                '        .TextBox1.Visible = True
                '        .Label1.Visible = True
                .ShowDialog()
            End With

        ElseIf Me.RadioButton2.Checked Then                  'PICTURE INPUT
            '      msg$ = "Show Picture for Password?"
            '      Style = vbOKCancel        ' Define buttons.
            '      Title$ = "Picture for Password Entry"
            '      Response = MsgBox(msg$, Style, Title$)
            '      If Response = vbCancel Then
            '          ComingBack = 1
            '          Exit Sub
            '      End If
            '      SHOWPICTURE()               '================================
            With dlgPublicKeyVisiblePassword
                .TextBox1.Visible = False
                .Label1.Visible = False
                .ShowDialog()
            End With
            '      dlgPublicKeyVisiblePW.Show vbModal
            'ElseIf Me.RadioButton3.Checked Then                 'ONSCREEN INPUT
            '    '      frmMyKeyBoard.Show vbModal
            '    With dlgPublicKeyVisiblePassword
            '        .TextBox1.Visible = False
            '        .Label1.Visible = False
            '        .ShowDialog()
            '    End With
            '      dlgPublicKeyVisiblePW.Show vbModal
        End If

        If ComingBack Then
            ComingBack = False
            Cursor.Current = System.Windows.Forms.Cursors.Default
            Exit Sub
        End If


        Cursor.Current = System.Windows.Forms.Cursors.WaitCursor
        GenerateInternalKey(PrivatePassword)
        PasswordInRamTimer.Enabled = True
        '      frmGeneratingKeys.Show()
        '      frmGeneratingKeys.Refresh()
        '      'Dim RawPrivateKeyFile$
        'PubKeyDate = Nothing
        PreviousPublicKeyExpired = ""
        Dim PublicKeyFile As String = OrigDirectory$ & "Keys\" & SendersEMail$ & ".PUB"
        Dim PrivateKeyFile As String = OrigDirectory$ & "Keys\" & SendersEMail$ & ".EPK"

        '
        '    doingPublicKey = True
        '    PubKeyDate = File.GetLastWriteTime(PublicKeyFile)
        '    PreviousPublicKeyExpired = "The previous Public Key that was sent to you on " & PubKeyDate & " has been superseded. Do not use it." & Paragraph
        'End If
        Dim strPublicKey As String = ""
        Dim intres As Integer
        'SenderPubKeyExists = GetSetting("winexpl", "startup", "PublicKey", SendersEMail)
        If File.Exists(OrigDirectory$ & "Keys\" & SendersEMail$ & ".txt") Then
            FileOpen(1, OrigDirectory$ & "Keys\" & SendersEMail$ & ".txt", OpenMode.Input)
            PubKeyDate = CDate(LineInput(1))
            FileClose(1)
            PreviousPublicKeyExpired = "The previous Public Key that was sent to you on " _
                & PubKeyDate & " has been superseded. Do not use it." & Paragraph
            intres = Rsa.MakeKeys(PublicKeyFile, PrivateKeyFile, 2048, Rsa.PublicExponent.Exp_EQ_5, 1000,
                                  InternalKey, CipherAlgorithm.Tdea, HashAlgorithm.Sha256, Rsa.Format.PEM, False)
            'Rabin Miller tests 64***********************************
            PubKeyDate = File.GetLastWriteTime(PublicKeyFile)
        Else
            intres = Rsa.MakeKeys(PublicKeyFile, PrivateKeyFile, 2048, Rsa.PublicExponent.Exp_EQ_5, 1000,
                                  InternalKey, CipherAlgorithm.Tdea, HashAlgorithm.Sha256, Rsa.Format.PEM, False)
            'Rabin Miller tests 64***********************************
            PubKeyDate = File.GetLastWriteTime(PublicKeyFile)
        End If




        SendCert = False
        strCertName = OrigDirectory$ & "Certs\" & RecipsEMail & ".CER"
        If Not File.Exists(strCertName) Then
            'Dim nRet As Integer
            Dim kuoKeyUsage As X509.KeyUsageOptions

            kuoKeyUsage = X509.KeyUsageOptions.DigitalSignature
            Dim result = MessageBox.Show("Since this is the first time you are sending " & SendersEMail & " your Public Key, your X509 Certificate will automatically be generated and sent first. After it goes, Select 1 - Email Public Key again.", "Your X509 Certificate", MessageBoxButtons.OKCancel)
            If result = Windows.Forms.DialogResult.Cancel Then
                Cursor.Current = System.Windows.Forms.Cursors.Default
                ComingBack = True
                Exit Sub
            End If
            Dim comName As String = "CN=" & RecipsEMail
            Dim DudeNeme As String = "GN=" & HisName
            X509.MakeCertSelf(strCertName, PrivateKeyFile, 99, 10, comName & ";" &
                              DudeNeme & ";O=BMC Engineering;OU=Away RJN Cryptography", "", kuoKeyUsage, InternalKey, 0)
            SendCert = True
            SendEMail(1, SendersEMail$, "Recipient's X509 Certificate.", "")
            Cursor.Current = System.Windows.Forms.Cursors.Default
            SendCert = False
            Exit Sub
        End If



        doingPublicKey = True




        Dim strHexHashThumb As String
        strHexHashThumb = X509.CertThumb(strCertName, HashAlgorithm.Sha1)
        'Console.WriteLine("X509_CertThumb returns " & strHexHash.Length & " for " & strCertName)
        'Console.WriteLine(strHexHash)

        If intres > 0 Then
            showRsaErrorCodes(intres)
            Me.Close()
            Exit Sub
        End If
        '*****Encrypting Private Key is Redundant **********
        'EncryptingPrivateKey = True
        'Dim SendersNme As String ', strPublicKey$, strXML$
        'GenerateInternalKey(PrivatePassword$)
        'Cmd = "ENCRYPT"
        ''      'FileCopy PrivateKeyFile, RawPrivateKeyFile
        'SendersNme = SendersEMail & ".EPK"
        'Encrypt(Cmd, OrigDirectory$ & "Keys\", SendersNme, 1)
        'EncryptingPrivateKey = False
        'End If

        '      ' Read in the deciphered public key string in our internal format
        'strPublicKey = RsaReadPublicKey(PublicKeyFile)    (vb6)
        Dim sb As New StringBuilder()
        sb = Rsa.ReadPublicKey(PublicKeyFile)
        strPublicKey = sb.ToString()
        If Len(strPublicKey) = 0 Then
            MsgBox("Unable to retrieve public key")
            ' DumpTheForm()
            Exit Sub
        End If
        '      'Debug.Print "INTKEY=" & strPrivateKey
        'Dim strXML As String
        '      ' Convert to XML
        RecipsPublicKey = Rsa.ToXMLString(strPublicKey, Rsa.XmlOptions.ForceRSAKeyValue)

        '  '<RsaKeyValue><Modulus>-----</Modulus><Exponent>BQ==</Exponent></RsaKeyValue>
        Dim Frontstr As String = "<RsaKeyValue><Modulus>"
        Dim Backstr As String = "</Modulus><Exponent>BQ==</Exponent></RsaKeyValue>"
        Dim dLen As Integer = Len(Backstr)
        RecipsPublicKey = Strings.Mid(RecipsPublicKey$, 23)
        Dim nChars As Integer = Len(RecipsPublicKey$)
        RecipsPublicKey = Strings.Left(RecipsPublicKey$, nChars - dLen)
        'use back 40 of RecipsPublicKey
        '*******************MAKING PUBLIC KEY HASH ********************************
        Dim PuKeyEnd As String = Strings.Right(RecipsPublicKey, 40)
        Dim PuKeyEndByte() As Byte = System.Text.UTF8Encoding.UTF8.GetBytes(PuKeyEnd)
        Dim HexHashThumbByte() As Byte = System.Text.UTF8Encoding.UTF8.GetBytes(strHexHashThumb)
        Dim PuKeyHash() As Byte = Nothing, PuKeyHashSTR As String = ""
        ReDim PuKeyHash(39)
        For pk = 0 To 39
            PuKeyHash(pk) = PuKeyEndByte(pk) Xor HexHashThumbByte(pk)
        Next
        PuKeyHashSTR = Cnv.ToBase64(PuKeyHash)
        nChars = Len(PuKeyHashSTR)                          ' 40 becomes = 56
        RecipsPublicKey = RecipsPublicKey & PuKeyHashSTR
        PubDateKey = CStr(PubKeyDate)
        PubDateKey = PubDateKey.PadRight(22, "0"c)

        PubDateKey = Cnv.ToBase64(PubDateKey)       '=32 bytes after b64
        dLen = Len(PubDateKey)
        LeftPart = Strings.Left(RecipsPublicKey, 18)
        RightPart = Strings.Mid(RecipsPublicKey, 19)
        RecipsPublicKey = LeftPart & PubDateKey & RightPart
        RecipsPublicKey = "From: " & RecipsEMail & Chr(149) & RecipsPublicKey

        SendEMail(0, SendersEMail$, "Recipient's Public Key", RecipsPublicKey$)
        Cursor.Current = System.Windows.Forms.Cursors.Default
        doingPublicKey = False
        Exit Sub
    End Sub



    Private Sub Button2_Click_2(sender As Object, e As EventArgs) Handles Button2.Click
        dismount()
    End Sub

    Private Sub LinkLabel5_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel5.LinkClicked
        'ENCRYPT
        'On Error GoTo 0
        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If

        'If NeedAutoEncrytTimer Then
        '    chkAutoEncrypt.Value = 0
        '    chkAutomaticDecrypt.Value = 0
        'End If
        'ComingBack = 0
        'AutoEncryptTimer.Enabled = False
        Cmd = "ENCRYPT"



        If ComingBack Then
            ComingBack = False
            Cursor.Current = System.Windows.Forms.Cursors.Default
            ' Cursor.Current = System.Windows.Forms.Cursors.Default
            Exit Sub
        End If

        Cursor.Current = System.Windows.Forms.Cursors.WaitCursor      'Default 
        'Cursor.Current = System.Windows.Forms.Cursors.WaitCursor      'Cursor = Cursors.WaitCursor
        ' ListView1.UseWaitCursor = True
        'GenerateInternalKey(PrivatePassword)
        'PasswordInRamTimer.Enabled = True

        Prepare()
    End Sub

    Private Sub LinkLabel6_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel6.LinkClicked
        'DECRYPT

        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If


        If NeedAutoEncrytTimer Then
            'chkAutoEncrypt.Value = 0
            'chkAutomaticDecrypt.Value = 0
        End If

        Cmd = "DECRYPT"



        If ComingBack Then
            ComingBack = False
            Cursor.Current = System.Windows.Forms.Cursors.Default
            Exit Sub
        End If
        Cursor.Current = System.Windows.Forms.Cursors.WaitCursor

        Prepare()
        Cursor.Current = System.Windows.Forms.Cursors.Default
    End Sub

    Private Sub LinkLabel8_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel8.LinkClicked
        'VIEW PICTURES
        GoViewer = True
        Cmd = "DECRYPT"
        StandardNoteEncrypting = True
        'MsgBox(String.Format("AvailablePhysicalMemory: {0} MBytes", System.Math.Round(My.Computer.Info.AvailablePhysicalMemory / (1024 * 1024)), 2).ToString)
        Prepare()

    End Sub

    Private Sub LinkLabel9_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel9.LinkClicked
        'frmAddorIncludeFolder
        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If

        'If NeedAutoEncrytTimer Then
        '    chkAutoEncrypt.Value = 0
        '    'chkAutomaticDecrypt.Value = 0
        'End If
        'TurnOffButtons()
        'Folder2ShowinCombo1Box$ = Combo1.Text
        Try
            frmAddorIncludeFolder.ShowDialog()
            FillTheList()
        Catch
            MessageBox.Show("You can not process files in Away RJN Cryptography" & vbCrLf & "while Windows Explorer or any other App (Photoshop, etc.)" & vbCrLf & "is accessing your Special Folder," & combItem & vbCrLf & "Close those Apps and continue", "Can't Access File.", MessageBoxButtons.OK)
            'If File.Exists(TentFile) Then
            '    Kill(TentFile)
            'End If
            ComingBack = True
            Exit Sub
        End Try
        'fsw.EnableRaisingEvents = True

        Exit Sub
    End Sub

    Private Sub LinkLabel10_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel10.LinkClicked
        ' delete folder
        'fsw.EnableRaisingEvents = False
        '        Dim efl As Integer, DriveLetter$, Drv%
        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If

        Dim response = MessageBox.Show("Deleting " & combItem & " from Special Folder List Only.", "Deleting Special Folder ", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation)


        If response <> vbOK Then    ' User chose Yes.

            'TurnOnButtons()
            FillTheList()
            Exit Sub
        End If
        If combItem <> "None Yet" Then
            '            If NeedAutoEncrytTimer Then
            '                chkAutoEncrypt.Value = 0
            '                chkAutomaticDecrypt.Value = 0
            '            End If


            Dim sr As StreamReader = File.OpenText(whichlist)
            Dim ef As Integer = 0, ExistingFolders(200) As String
            Do Until sr.Peek = -1
                ef += 1
                ExistingFolders(ef) = sr.ReadLine
                If ExistingFolders(ef) = "None Yet" Then
                    Exit Sub
                End If

            Loop
            sr.Close()
            If ShowingFlashFile = True Then
                combItem = FlashDriveLetter & CStr(ComboBox1.SelectedItem)
            End If
            Dim sw As StreamWriter = File.CreateText(whichlist)
            For jk = 1 To ef
                If ExistingFolders$(jk) = combItem Then
                    ExistingFolders$(jk) = ""
                Else
                    '            Print #1, ExistingFolders$(efl)
                    sw.WriteLine(ExistingFolders$(jk))
                End If
            Next
            sw.Close()
            DoTheBox()



            '        TurnOffButtons()
            '        msg$ = "Deleting   " & Folder2ShowinCombo1Box$               ' Define message.
            '        Style% = vbYesNo             ' Define buttons.
            '        Title$ = "Deleting Folder From Special List"
            '        ' Display message.
            '        Response = MsgBox(msg$, Style%, Title$)
            '        If Response <> vbYes Then    ' User chose Yes.
            '            TurnOnButtons()
            '            Exit Sub
            '        End If




        End If


        '        TurnOnButtons()
        'fsw.EnableRaisingEvents = True
    End Sub

    Private Sub LinkLabel11_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel11.LinkClicked
        'rename file
        Dim HighlightedGroup As ListView.SelectedListViewItemCollection = Me.ListView1.SelectedItems
        Dim item As ListViewItem
        Dim originalFileName As String = ""

        If AutoEncryptTimerStillDoing Then
            Exit Sub
        End If
        If HighlightedGroup.Count > 0 Then
            For Each item In HighlightedGroup
                If HighlightedGroup.Count = 1 Then
                    originalFileName = Path.GetFileNameWithoutExtension(item.Text)
                    Tail = Path.GetExtension(item.Text)
                    RenameFile.TextBox1.Text = originalFileName
                Else
                    ComingBack = True
                    Exit Sub
                End If
            Next
        Else
            ComingBack = True
            Exit Sub
        End If
        RenameFile.ShowDialog()
        If ComingBack = True Then
            Exit Sub
        End If
        Dim NewName As String = RenameFile.TextBox1.Text
        If NewName <> originalFileName Then
            My.Computer.FileSystem.RenameFile(combItem & originalFileName & Tail, NewName & Tail)
            FillTheList()
        End If
    End Sub

    Private Sub LinkLabel12_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel12.LinkClicked
        'delete files
        'fsw.EnableRaisingEvents = False
        PermDeleteFiles()
    End Sub
    Private Sub PermDeleteFiles()
        Dim counter = My.Computer.FileSystem.GetFiles(combItem)
        Dim FileNumber As Integer = counter.Count
        Dim FileToBeDeleted(FileNumber) As String

        ''            TurnOffButtons()

        '        If AutoEncryptTimerStillDoing Then
        '            Exit Sub
        '        End If

        '        'ListView1.HideSelection = False
        '        If NeedAutoEncrytTimer Then
        '            chkAutoEncrypt.Value = 0
        '            chkAutomaticDecrypt.Value = 0
        '        End If

        If combItem <> "None Yet" Then
            Cursor.Current = System.Windows.Forms.Cursors.WaitCursor
            Dim FileBooleanMarked(FileNumber) As Boolean
            Dim Status As Integer
            Dim selectedCount As Integer
            For i As Integer = ListView1.SelectedIndices.Count - 1 To 0 Step -1
                Status = ListView1.SelectedIndices.Item(i)
                FileBooleanMarked(Status) = True
                selectedCount += 1
            Next
            Dim SmalCount As Integer = 0
            For itmX = 0 To FileNumber
                If FileBooleanMarked(itmX) = True Then
                    Dim lvi As ListViewItem = Me.ListView1.SelectedItems(SmalCount)
                    SmalCount += 1
                    FileToBeDeleted(SmalCount) = combItem & lvi.SubItems(0).Text
                End If
            Next
            FileClose()

            For jx = 1 To SmalCount
                SmashPicture(FileToBeDeleted(jx))
            Next

            '            'istView1.HideSelection = False
            '            msg$ = "Scramble/Delete Highlighted Files?"
            '            Style% = vbYesNo          ' Define buttons.
            '            Title$ = "Deleting Files From Special Folder"
            '            ' Display message.
            '            Response = MsgBox(msg$, Style%, Title$)
            '            'ListView1.HideSelection = True
            '            If Response <> vbYes Then ' User chose Yes.

            '                frmAWAY32.FillTheList()
            '                Exit Sub
            '            End If
            '            Dim itmX As ListItem
            '            Screen.MousePointer = vbHourglass
            '            

            '            frmAWAY32.FillTheList()
            '            frmAWAY32.Show()
            '            Screen.MousePointer = vbDefault

            FillTheList()
        End If
        'Cursor.Current = System.Windows.Forms.Cursors.Default
        'fsw.EnableRaisingEvents = True
        '        Exit Sub
        'SelectFil:
        '        Resume Next
        '        frmAWAY32.GetStarted()
        '        Exit Sub
    End Sub

    Public Shared Function RemoveAttribute(ByVal attributes As FileAttributes, ByVal attributesToRemove As FileAttributes) As FileAttributes
        Return attributes And (Not attributesToRemove)
    End Function

    Private Sub LinkLabel7_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel7.LinkClicked
        'Verify Your Recipient
        VerifyRecipient.ShowDialog()
    End Sub


    Private Sub LinkLabel13_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel13.LinkClicked
        'Write Message, Standard Encrypt it,then Email it. 

        'Determine is TBird is present

        'If NoTBird Then

        '    NoticeTBird.ShowDialog()
        '    If ComingBack = True Then
        '        ComingBack = False
        '        Cursor.Current = System.Windows.Forms.Cursors.Default
        '        Exit Sub
        '    End If
        'End If
        'Standard Encrypt AwayRJNWriter note
        Cmd = "ENCRYPT"
        TypeofKey()
        StandardNoteEncrypting = True
        If ComingBack = True Then
            ComingBack = False
            StandardNoteEncrypting = False
            Exit Sub
        End If
        frmMainWriter.LinkLabel2.Visible = False
        frmMainWriter.ShowDialog()
        If ComingBack = True Then
            ComingBack = False
            StandardNoteEncrypting = False
            Exit Sub
        End If
        DoTheNoteEmail()
    End Sub
    Public Sub DoTheNoteEmail()
        'SendEMail(AttachFiles%, SendTo$, MsgSubject$, CiphContent$)
        If NoTBird Then

            NoticeTBird.ShowDialog()
            If ComingBack = True Then
                ComingBack = False
                Cursor.Current = System.Windows.Forms.Cursors.Default
                Exit Sub
            End If
        End If
        SendEMail(qtyAttach, RecipTo, SubjeText, bodConten)
        StandardNoteEncrypting = False
    End Sub




End Class
