{-# OPTIONS_GHC -fno-warn-tabs #-}
import Data.Char

-- Type Datas -------------------------------------

data StorageDevice = StorageDevice { pathDev :: String
                     , sizeDev :: String
                     , manageByLVM :: Bool
                     } deriving (Show)

data User = User { userName :: String
                     , userID :: Int
                     , primaryGroup :: String
                     , secundaryGroups :: [String]
                     , homeDirectory :: String
                     } deriving (Show, Eq)

data Group = Group { groupName :: String
                     , groupID :: Int
                     , associatedUsers :: [User]
                     } deriving (Show, Eq)

data FileSystem = FileSystem { fsName :: String
                     , fsSize :: String
                     , mountedOn :: String
                     } deriving (Show)

data VolumeGroup = VolumeGroup { logVolList :: [String]
                     , physVolList :: [String]
                     , vgSize :: String
                     , freeVgSize :: String
                     } deriving (Show)

data FilesAndDirectories = FilesAndDirectories
                     { user :: String
                     , group :: String
                     , hour :: String
                     , fileName :: String
                     , flagSymbLink :: Bool
                     } deriving (Show)

data DataMaster = DataMaster 
                     { users :: [User]
                     , groups :: [Group]
                     , stgdevices :: [StorageDevice]
                     , filesys :: [FileSystem]
                     , volgroup :: [VolumeGroup]
                     , fileanddir :: [FilesAndDirectories]
                     } deriving (Show)

-- Type Datas -------------------------------------
     

-- Users and Groups  ###############################

-- | Check size of a String
sizeTest :: [Char] -> Int -> Bool
sizeTest word size =
	if (((length word) <= size) && (word /= "")) then True else False

-- | Check some requirements
allCorrect :: [Char] -> Bool
allCorrect word =
	if word == "" then True else
	if isLower(head word) then allCorrect (tail word) else
	if isDigit(head word) then allCorrect (tail word) else False

verifyUser :: [Char] -> Bool
verifyUser user =
	if(sizeTest(user) (32) && allCorrect(user)) 
		then (True)
		else (False)

verifyGroup :: [Char] -> Bool
verifyGroup group = 
	if(sizeTest(group) (32) && allCorrect(group))
		then True
		else False

editDataMasterGroup :: DataMaster -> [Group] -> IO()
editDataMasterGroup dataMaster group =
	menu DataMaster {users=(getUsers dataMaster), 
						groups=(getGroups dataMaster)++(group), 
						stgdevices=(getStgDev dataMaster),
						filesys=(getFileSys dataMaster),
						volgroup=(getVolGr dataMaster),
						fileanddir=(getFileDir dataMaster)}

editGroup :: Group -> User -> Group
editGroup group user = Group {groupName=groupName(group), 
								groupID=groupID(group),
								associatedUsers=associatedUsers(group)++[user]}

searchGroup :: String -> [Group] -> Group
searchGroup groupToSearch groups =
	if groupToSearch == groupName(groups!!0) then groups!!0 
		else
			searchGroup (groupToSearch) (tail groups)

makeListGroupsToUpdate :: User -> [Group] -> [Group]
makeListGroupsToUpdate user groups = 
	(makeListGroupsToUpdateAux ([primaryGroup(user)]) (groups) ([])) ++ (makeListGroupsToUpdateAux (secundaryGroups(user)) (groups) ([]))
	

makeListGroupsToUpdateAux :: [String] -> [Group] -> [Group] -> [Group]
makeListGroupsToUpdateAux groupsToSearch groups answer =
	if groupsToSearch == [] then answer
		else makeListGroupsToUpdateAux (tail groupsToSearch) (groups) (answer++[searchGroup (groupsToSearch!!0) (groups)] )

updateGroups :: User -> [Group] -> [Group] -> [Group]
updateGroups user listUpdate groups =
	if listUpdate == [] 
		then groups 
		else updateGroupsAux (user) (listUpdate) (groups) ([])

updateGroupsAux :: User -> [Group] -> [Group] -> [Group] -> [Group]
updateGroupsAux user groupToUpdate groups answer =
	if groups == [] then updateGroups (user) (tail groupToUpdate) (answer) else
	if groupName(groupToUpdate!!0) == groupName(groups!!0) 
		then updateGroupsAux (user) (groupToUpdate) (tail groups) (answer++[editGroup (groups!!0) (user)])
		else updateGroupsAux (user) (groupToUpdate) (tail groups) (answer++[groups!!0])

editDataMasterUser :: DataMaster -> User -> IO()
editDataMasterUser dataMaster user = do
	menu DataMaster {users=(getUsers dataMaster)++([user]), 
										groups=(updateGroups (user) (makeListGroupsToUpdate (user) (getGroups(dataMaster))) (getGroups(dataMaster))), 
										stgdevices=(getStgDev dataMaster),
										filesys=(getFileSys dataMaster),
										volgroup=(getVolGr dataMaster),
										fileanddir=(getFileDir dataMaster)}

-- | Create group if is possible
createGroup :: [String] -> DataMaster -> IO()
createGroup command dataMaster =
	(if (length(command) == 2) then 
	(if (sizeTest (last command) 32)
		then (if (allCorrect (last command)) 
			then do
				editDataMasterGroup dataMaster [Group {groupName=command!!1, 
								groupID=(1000+length(getGroups dataMaster)),
								associatedUsers=[]}] 
				else do 
					putStrLn "Error"
					menu dataMaster) 
		else do
			putStrLn "Error"
			menu dataMaster) 
	else do 
		putStrLn "Error"
		menu dataMaster)

dontGroupExist :: [Group] -> String -> Bool
dontGroupExist groups name =
	if groups == [] then False else
	if(groupName(groups!!0) == name) then True else
	dontGroupExist (tail groups) name

addUser :: DataMaster -> [String] -> IO()
addUser dataMaster command =
	if( (dontGroupExist (getGroups(dataMaster)) (command!!2)) ) then 
	do
		(editDataMasterUser (dataMaster) (User { userName=(last command)
					                     , userID=(1000+length(getUsers(dataMaster)))
					                     , primaryGroup=command!!2
					                     , secundaryGroups=[]
					                     , homeDirectory="/home/"++(last command)
					                     }))
		else do
			putStrLn "Error group doesn't exist"
			menu dataMaster 

addUserWith2ndGroups :: DataMaster -> [String] -> IO()
addUserWith2ndGroups dataMaster command =
	if( (dontGroupExist (getGroups(dataMaster)) (command!!2)) ) then 
	do
		(editDataMasterUser (dataMaster) (User { userName=(last command)
					                     , userID=(1000+length(getUsers(dataMaster)))
					                     , primaryGroup=command!!2
					                     , secundaryGroups=getGroupName(command!!4)("")([])
					                     , homeDirectory="/home/"++(last command)
					                     }))
		else do
			putStrLn "Error group doesn't exist"
			menu dataMaster 

user4param :: [String] -> DataMaster -> IO()
user4param command dataMaster = 
	if(	(tail(command))!!0 == "-g" &&
		verifyGroup((tail(command))!!1) &&
		verifyUser((tail(command))!!2)) 
	then( addUser dataMaster command ) else ( putStrLn "Error parameters" )

group2aryExists :: [String] -> DataMaster -> Bool
group2aryExists groups dataMaster =
	if groups == [] then True else
	if dontGroupExist (getGroups dataMaster) (groups!!0) 
		then group2aryExists (tail groups) (dataMaster)
		else False

getGroupName :: String -> String -> [String] -> [String]
getGroupName name word answer =
	if name == "" then answer++[word] else
	if name!!0 == ',' then getGroupName (tail name) ("") (answer++[word]) 
		else do
			getGroupName (tail name) (word++[name!!0]) (answer)

verify2aryGroups :: String -> DataMaster -> Bool
verify2aryGroups list dataMaster =
	if group2aryExists (getGroupName (list) ("") ([])) dataMaster 
		then True else False

user6param :: [String] -> DataMaster -> IO()
user6param command dataMaster =
	(if( (tail command)!!0 == "-g" &&
			verifyGroup( (tail command)!!1 ) &&
			(tail command)!!2 == "-G" && 
			verifyUser( last( (tail command) ))) 
		then( if( (verify2aryGroups (command!!4) (dataMaster)) ) 
					then( addUserWith2ndGroups dataMaster command ) 
					else( putStrLn "Error 6 parameters" ) ) 
		else do 
			putStrLn "Error 6 parameters"
			menu dataMaster )

-- | Create user if is possible
createUser :: [String] -> DataMaster -> IO()
createUser command dataMaster =
	if (length(command) == 4) 
		then( user4param (command) (dataMaster) )
	else if ( length(command) == 6 )
		then( user6param (command) (dataMaster)) 
			else do
				putStrLn "Error input"
				menu dataMaster

printList :: [User] -> String -> String
printList list str =
	if list == [] then "vacio" else
	do
		printList (tail list) (str++userName(list!!0)++", ")

printGroups :: [Group] -> DataMaster -> IO()
printGroups groups dataMaster =
	if groups == [] then menu dataMaster else
	do
		putStrLn(groupName(groups!!0)++"        "++show(groupID(groups!!0))++printList(associatedUsers(groups!!0))("     "))
		printGroups (tail groups) (dataMaster)

print2aryUserGroups :: [String] -> String -> String
print2aryUserGroups groups answer =
	if groups == [] then answer 
		else
			print2aryUserGroups (tail groups) (answer++","++groups!!0)


printUsers :: [User] -> DataMaster -> IO()
printUsers users dataMaster =
	if users == [] then menu dataMaster else
	do
		putStrLn(userName(users!!0)++"      "++
					show(userID(users!!0))++"     "++
					primaryGroup(users!!0)++"     "++
					print2aryUserGroups(secundaryGroups(users!!0))("")++"     "++
					homeDirectory(users!!0) )
		printUsers (tail users)(dataMaster)

showListGroupsUsers :: [String] -> DataMaster -> IO()
showListGroupsUsers command dataMaster =
	if(length command == 1 || length(command) > 2) 
		then do 
			putStrLn "Error"
			menu dataMaster 
			else
	if(command!!1 == "groups") then do
		putStrLn "GroupName      GID      AssociattedUsers"
		printGroups (getGroups dataMaster)(dataMaster)
		menu dataMaster 
		else
	if(command!!1 == "users") then do 
		putStrLn "UserName      UID      PrimaryGroup      SecundaryGroups      HomeDirectory"
		printUsers (getUsers dataMaster)(dataMaster)
		menu dataMaster 
		else do 
			putStrLn "Error" 
			menu dataMaster

userExist :: [User] -> String -> Bool
userExist users name =
	if users == [] then False else
	if userName(users!!0) == name then True
		else
			userExist (tail users) (name)

findUser :: [User] -> String -> User
findUser users name =
	if userName(users!!0) == name then (users!!0) 
		else
			findUser (tail users) (name)

printFinger :: User -> DataMaster ->IO()
printFinger user dataMaster = 
	do 
		putStrLn ("UserName: "++(userName(user)))
		putStrLn ("UID: "++show(userID(user)))
		putStrLn ("HomeDirectory: "++(homeDirectory(user)))
		putStrLn ("Associated primary group: "++(primaryGroup(user)))
		putStrLn ("Associated secondary groups: " )
		mapM_ putStrLn (secundaryGroups(user))
		menu dataMaster 

fingerUser :: [String] -> DataMaster -> IO()
fingerUser command dataMaster =
	if(length(command) == 1 || length(command) > 2) 
		then do 
			putStrLn "Error finger input"
			menu dataMaster
		else 
			if(verifyUser(command!!1)) 
			then ( if(userExist(getUsers(dataMaster))(command!!1)) 
				then 
					printFinger (findUser(getUsers(dataMaster)) (command!!1)) dataMaster 
				else(putStrLn "Error user not exist") ) 
			else putStrLn "Error finger input"

userDel :: [String] -> Bool
userDel command =
	if(length(command) == 1 || length(command) > 2) then False else
	if(verifyUser(command!!1)) then True else False

groupDel :: [String] -> Bool
groupDel command =
	if(length(command) == 1 || length(command) > 2) then False else
	if(verifyGroup(command!!1)) then True else False

-- FALTA TRATAR CON LA LISTA DE ENTRADA!!!!!!!!!
modUser :: [String] -> String -> Bool
modUser commandUser userParam =
	if(commandUser == []) then True else
	if(commandUser!!0 == "-g") then modUser (tail commandUser) userParam else
	if(commandUser!!0 == "-G") then modUser (tail commandUser) userParam 
	else False

userModification :: [String] -> Bool
userModification command =
	if(length(command) == 2) then True else
	if(length(command) >= 4) 
		then ( if(verifyUser(last command)) 
			then( modUser(tail(init command)) (last command) ) 
			else(False) )
		else (False) 

-- Users and Groups ###############################

-- Volume Groups ##################################

pvcreateFunction :: [String] -> Bool
pvcreateFunction command = 
	if(length(command) == 2) 
		then( if( (take 5(command!!1)) == "/dev/" &&
					(drop 5(command!!1)) /= "" ) 
				then(True) 
				else(False)) 
	else False

verifyVGName :: String -> Bool
verifyVGName name =
	if( sizeTest(name) (32) && allCorrect(name) ) then True else False

vgcreateAux :: [String] -> String -> Bool
vgcreateAux devs name =
	if devs == [] then True else
	if ((take 5(devs!!0)) == "/dev/" && (drop 5(devs!!0)) /= "") 
		then ( vgcreateAux (tail devs) (name) )
	else (False)

vgcreateFunction :: [String] -> Bool
vgcreateFunction command =
	if(length(command) >= 3) 
		then( if( verifyVGName(command!!1) ) 
			then( vgcreateAux(drop 2(command)) (command!!1)) 
			else( False ) ) 
	else(False)

vgreduceFunction :: [String] -> Bool
vgreduceFunction command =
	if(length(command) == 3) 
		then( if( verifyVGName(command!!1) ) 
				then( if( (take 5(command!!2)) == "/dev/" &&
							(drop 5(command!!2)) /= "" ) 
					then(True) 
					else(False) ) 
				else(False) ) 
	else(False)

vgextendFunction :: [String] -> Bool
vgextendFunction command =
	if(length(command) == 3) 
		then( if( verifyVGName(command!!1) ) 
				then( if( (take 5(command!!2)) == "/dev/" &&
							(drop 5(command!!2)) /= "" ) 
					then(True) 
					else(False) ) 
				else(False) ) 
	else(False)

vgDisplay :: [String] -> Bool
vgDisplay command =
	if(length(command) == 1) 
		then( True ) 
	else if(length(command) == 2) 
		then( if( command!!1 == "-v" ) 
			then( True ) 
			else( if( verifyVGName(command!!1) ) 
				then( True ) else(False) ) ) 
	else if(length(command) == 3) 
		then( if(command!!1 == "-v" && verifyVGName(command!!2)) 
			then( True ) 
			else( False ) ) 
		else( False )

vgRemove :: [String] -> Bool
vgRemove command =
	if( length(command) == 2 ) 
		then( if( verifyVGName(command!!1) ) 
			then( True ) 
			else( False ) ) 
		else( False )

verifyLVName :: String -> Bool
verifyLVName name =
	if( sizeTest(name) (32) && allCorrect(name) ) then True else False

lvCreate :: [String] -> Bool
lvCreate command =
	if(length(command) >= 6) 
		then( if( command!!1 == "-L" && command!!3 == "-n" &&
			verifyLVName(command!!4) && verifyVGName(command!!5)) 
			then( if (last(command!!2) == 'G' || last(command!!2) == 'M') 
				then( True ) else( False )) 
				else( False ) ) 
		else( False )  

-- Volume Groups ##################################

-- Storage Devices ##################################

-- | Check the size number
{-verifyNumber :: String -> Bool
verifyNumber num = 
	(if (num == "")
	 then True 
	 else ( if(isDigit (head (num))) 
	 	    then(verifyNumber (drop 1(num)))
	        else(False) ) )


-- | Check some requirements about the device's size
sizeDev :: String -> Bool
sizeDev dev = 
    (if (init(dev) /= "0") && (verifyNumber(init(dev)))
    then ( if ( (last(dev) == 'G') || (last(dev) == 'M') )
           then True 
           else False)
    else False) 

-- | Check some requirements about the device path
isCorrectPath :: [Char] -> Bool
isCorrectPath path = 
	(if (take 5(path) == "/dev/") 
	 then ( if (null (drop 5(path)) )
	         then False
	         else True ) 
	 else False)

-- | Check if the command is correct 
verifyCommand :: [String] -> Bool
verifyCommand cmd = 
	if( cmd!!0 == "createdev" && cmd!!1 == "-s" ) 
		then( if( sizeDev(cmd!!2) ) 
			then( if(isCorrectPath(last(cmd)) ) 
				then(True) else(False)) else(False)) else(False)


storeDeviceData :: [String] -> [] -> Bool 
storeDeviceData cmd dataList = 
	(if (length (cmd) == 2) 
	 then (devDataToStorage dataList)
 	 else (storeDeviceData (init(cmd)) (dataList ++ last(cmd)))) )


-- | Create storage devices
createStorageDevice :: [String] -> Bool
createStorageDevice cmd =
	(if (length(cmd) == 4)
	 then ( if ( verifyCommand(cmd) ) 
	       then (storeDeviceData cmd [])
	  	   else	False )
	 else False)

-- | Print the device list
printDevList :: [String] -> Bool
printDevList cmd = 
	  if(length(cmd) == 2) 
	  then ( (if (cmd!!1 == "-l")
	  	    then True
	  	    else False) )
	  -- | PRINT
	  else (False)

-- | Remove a device 
deleteDevice :: [String] -> Bool 
deleteDevice cmd = 
	if(length(cmd) == 2) 
	then ( if( (take 5(cmd!!1)) == "/dev/" &&
	                  (drop 5(cmd!!1)) /= "")
	       then True 
	       else False ) 
	else(False)   -}

-- Storage Devices ##################################

-- File Systems ##################################

-- | verify if the fs path is correct
{-verifyPath :: String -> Bool
verifyPath name = 
	 if ( (((take 7(name)) == "vgName/") &&
	                 ((drop 7(name)) /= "") ) )
	 then True 
	 else ( (if(null name)
	         then (False) 
	         else (True)) ) 

-- | Create a new filesystem
createFileSystem :: [String] -> Bool
createFileSystem cmd = 
	(if (length (cmd) == 4)
	 then ( (if (cmd!!1 == "-t")
	         then ( (if( (cmd!!2 == "ext2") || (cmd!!2 == "ext3")
	                                  ||(cmd!!2 == "ext4") ) 
	                 then( (if( (take 5(cmd!!3)) == "/dev/" ) 
	 	     	            then( verifyPath (drop 5(cmd!!3)) ) 
	 	     	            else False) )  
	 	             else False) ) 
	         else False) ) 
	 else False)


-- | create the mount directory
createMountPoint :: [String] -> Bool
createMountPoint cmd =
	(if(length (cmd) == 3) 
	 then( (if( (cmd!!1 == "-p") && (cmd!!2 == "/mount/point")) 
	 	    then( True ) 
	 	    else(False))  ) 
	 else(False) )

-- | check if the path components exist
checkPathStatus :: String -> Bool
checkPathStatus path = 
	(if (take 5(path)) == "/dev/" )
	 then ( (if()
	 	     then()
	 	     else()) )
	 else(False))


-- | mount a file system
mountFileSystem :: [String] -> Bool
mountFileSystem cmd = 
	(if (length(cmd) == 3)
	then ( (if( cmd!!2 == "/mount/point")
		    then(checkPathStatus (cmd!!1))
		    else(False)) )
	else False)

-- | print the file systems mounted
printMountList :: [String] -> Bool
printMountList cmd = 



-- | other way to print the fs list
alternatePrintMount :: [String] -> Bool
alternatePrintMount cmd = 
	--| access to the devices data structure



-- | print the specific skills about some file system mounted
specificAlternatePrintMount :: [String] -> Bool
specificAlternatePrintMount cmd = 
	(if( (cmd!!1 == "-h") && (cmd!!2 == "/mount/point") )
	then(True)
	else(False))
	-- | find the specific dev and show your characteristics


umountFileSystem :: [String] -> Bool
umountFileSystem cmd = 
	(if(length(cmd) == 2)
	 then( (if( cmd!!1 == "/mount/point")
	 	    then(True)
	 	    else(False)) )
	 else(False))  -}

-- File Systems ##################################

-- Symbolic Links ##################################

-- | Manage the command according to the symb link operation
{-  manageSymbolicLink :: [String] -> Bool
manageSymbolicLink cmd = 
	if (cmd!!1 == "-s")
		then()
		else()  -}

-- Symbolic Links ##################################

getUsers :: DataMaster -> [User]
getUsers dataMaster = users dataMaster

getGroups :: DataMaster -> [Group]
getGroups dataMaster = groups dataMaster

getStgDev :: DataMaster -> [StorageDevice]
getStgDev dataMaster = stgdevices dataMaster

getFileSys :: DataMaster -> [FileSystem]
getFileSys dataMaster = filesys dataMaster

getVolGr :: DataMaster -> [VolumeGroup]
getVolGr dataMaster = volgroup dataMaster

getFileDir :: DataMaster -> [FilesAndDirectories]
getFileDir dataMaster = fileanddir dataMaster

main :: IO()
main = menu DataMaster {users=[], groups=[], stgdevices=[], filesys=[], volgroup=[], fileanddir=[]}

-- Main ------------------------------------------
menu :: DataMaster -> IO()
menu dataMaster = do
	putStr "# "
	input <- getLine
	if null input
		then
			menu dataMaster
	else do
		let command = words(input)
		------------------------------------------
		if (head(command) == "groupadd")
		then do
			createGroup command dataMaster
		------------------------------------------
		else if (head(command) == "useradd")
			then do
				createUser command dataMaster
		------------------------------------------
		else if (head(command) == "show")
			then do
				showListGroupsUsers command dataMaster
		------------------------------------------
		else if (head(command) == "finger")
			then do
				fingerUser command dataMaster
		------------------------------------------
		else if (head(command) == "userdel")
			then do
				if (userDel command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else if (head(command) == "groupdel")
			then do
				if (groupDel command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		-- NO FUNCIONA BIEN! COMPROBAR UNA VEZ EXISTAN LAS LISTAS
		else if (head(command) == "usermod")
			then do
				if (userModification command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else if (head(command) == "pvcreate")
			then do
				if (pvcreateFunction command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else if (head(command) == "vgcreate")
			then do
				if (vgcreateFunction command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else if (head(command) == "vgreduce")
			then do
				if (vgreduceFunction command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else if (head(command) == "vgextend")
			then do
				if (vgextendFunction command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else if (head(command) == "vgdisplay")
			then do
				if (vgDisplay command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else if (head(command) == "vgs")
			then do
				if (vgDisplay command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else if (head(command) == "vgremove")
			then do
				if (vgRemove command)
					then do
						putStrLn $ "OK"
						menu dataMaster
					else do
						putStrLn $ "Error"
						menu dataMaster
		------------------------------------------
		else do
		putStrLn $ "Error"
		menu dataMaster
	
-- Main -------------------------------------------