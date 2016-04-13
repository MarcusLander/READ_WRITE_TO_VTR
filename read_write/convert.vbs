on error resume next

wscript.echo ""

Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objShell = CreateObject( "Shell.Application" )

Set objFolder = objShell.BrowseForFolder( WINDOW_HANDLE, strPrompt, numOptions, strPath )
	if objFolder is nothing then
		Wscript.Echo "Operation cancelled by user."
		Wscript.quit
	end if

set objFolderItem = objFolder.Self
	objStartFolder = objFolderItem.Path

Set objFolder = objFSO.GetFolder(objStartFolder)

Set colFiles = objFolder.Files

i=0

if objFolder.Files.Count = 0 then
	Wscript.Echo "No Files in this folder"
	Wscript.quit
else
	for each objFile in colFiles
		If UCase(objFSO.GetExtensionName(objFile.name)) = "DAT" Then
			i = i+1
		End If
		
		if i = 0 then
			Wscript.Echo "Error: There are no *.dat files in " & objFolder.path
			Wscript.quit
		end if
	next
	
	Wscript.Echo "Preparing to rename " & i & " data files."
	Wscript.Echo ""
	Wscript.Echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	Wscript.Echo "!!! CAUTION: This script blindly converts all *.dat files in    !!!"
	Wscript.Echo "!!! the chosen directory, and it cannot differentiate bewteen   !!!"
	Wscript.Echo "!!! experimental data files and other *.dat files. If there     !!!"
	Wscript.Echo "!!! are any *.dat files other than the experimental data files, !!!"
	Wscript.Echo "!!! close this script now and remove them. Once renamed, this   !!!"
	Wscript.Echo "!!! action cannot be undone.                                    !!!"
	Wscript.Echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	Wscript.Echo ""
	Wscript.Echo "Are you sure you want to continue?"
	Wscript.Echo "    1 - Continue"
	Wscript.Echo "    2 - Quit"
	Wscript.Echo ""
	Wscript.StdOut.Write "Input: "
	strInput = Wscript.StdIn.ReadLine
	Wscript.Echo ""
	if strInput = "2" then
		Wscript.quit
	elseif strInput = "1" then
		'do nothing
	else
		Wscript.Echo "Not a valid input. The program will now stop... "
		Wscript.quit
	end if
	
end if

i=0

For Each objFile in colFiles
	If UCase(objFSO.GetExtensionName(objFile.name)) = "DAT" Then
		i=i+1
		oldName = objFile.name
		objFile.name = "PIV_input" & i & ".dat"
		'Wscript.Quit
		if err.number <> 0 then
			Wscript.Echo "Error #" & err.number & " when performing " & oldName & " -> PIV_input" & i & ".dat conversion."
			Wscript.Echo err.Description
			Wscript.quit
		end if
		Wscript.Echo oldName & " -> PIV_input" & i & ".dat"
	End If
Next

Wscript.Echo "Operation completed successfully."