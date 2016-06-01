{-# OPTIONS_GHC -fno-warn-tabs #-}
import Data.Char
import Data.Time

-- Type Datas -------------------------------------

data StorageDevice = StorageDevice { pathDev :: String
                     , sizeDev :: String
                     , size :: Int
                     , prefix :: String 
                     , manageByLVM :: Bool
                     } deriving (Show)

data User = User { userName :: String
                     , userID :: Int
                     , primaryGroup :: Group
                     , secundaryGroups :: [Group]
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

data File = File { nameFile :: String
                     , content :: String
                     , pathFile :: String
                     , creationDateFile :: String
                     , creationTimeFile :: String
                     , userFile :: String
                     , groupFile :: String
                     } deriving (Show, Eq)

data Directory = Directory { nameDirectory :: String
                     , pathDirectory :: String
                     , creationDateDirectory :: String
                     , creationTimeDirectory :: String
                     , userDirectory :: String
                     , groupDirectory :: String
                     , directories :: [Directory]
                     , files :: [File]
                     , symbolics :: [SymbolicLink]
                     } deriving (Show, Eq)

data SymbolicLink = SymbolicLink { nameSymbolic :: String
                     , pathSymbolic :: String
                     , pathToLink :: String
                     , creationDateSymbolic :: String
                     , creationTimeSymbolic :: String
                     , userSymbolic :: String
                     , groupSymbolic :: String
                     } deriving (Show, Eq)

data DataMaster = DataMaster 
                     { users :: [User]
                     , groups :: [Group]
                     , stgdevices :: [StorageDevice]
                     , filesys :: [FileSystem]
                     , volgroup :: [VolumeGroup]
                     , fileanddir :: (Directory,Directory)
                     } deriving (Show)

-- Type Datas -------------------------------------

-- Generic  ########################################

getTime :: IO String
getTime = do
	now <- getCurrentTime
	timezone <- getCurrentTimeZone
	return (formatTime defaultTimeLocale "%R" (utcToLocalTime timezone now))

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

getFileDir :: DataMaster -> (Directory,Directory)
getFileDir dataMaster = fileanddir dataMaster

getRootDirectory :: DataMaster -> Directory
getRootDirectory dataMaster = fst(getFileDir(dataMaster))

getActualPosition :: DataMaster -> Directory
getActualPosition dataMaster = snd(getFileDir(dataMaster))

defaultRootGroup :: Group
defaultRootGroup = Group {groupName="root", 
						groupID=1000,
						associatedUsers=[]}

defaultDirectory :: Directory
defaultDirectory = Directory {nameDirectory="/"
	             , pathDirectory = "/"
	             , creationDateDirectory = "date"
	             , creationTimeDirectory = "time"
	             , userDirectory = "root"
	             , groupDirectory = "root"
	         	 , directories = []
	         	 , files = []
	         	 , symbolics = []} 

menuDefault :: DataMaster -> IO()
menuDefault dataMaster = createUser (words("useradd -g root root")) (dataMaster)

-- Generic  ########################################     

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
		else searchGroup (groupToSearch) (tail groups)

searchGroups :: [String] -> [Group] -> [Group] -> [Group]
searchGroups groupsToSearch groups answer =
	if groupsToSearch == [] then answer 
		else searchGroups (tail groupsToSearch) (groups) (answer++[searchGroup(groupsToSearch!!0)(groups)])

makeListGroupsToUpdate :: User -> [Group]
makeListGroupsToUpdate user = []++[primaryGroup(user)]++secundaryGroups(user) 

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
					groups=(updateGroups (user) (makeListGroupsToUpdate (user)) (getGroups(dataMaster))), 
					stgdevices=(getStgDev dataMaster),
					filesys=(getFileSys dataMaster),
					volgroup=(getVolGr dataMaster),
					fileanddir=(getFileDir dataMaster)}

editDataMasterUserDelete :: DataMaster -> String -> [User] -> IO()
editDataMasterUserDelete dataMaster user users =
	menu DataMaster {users=users, 
					groups=updateGroupsDeleteUser (user) (getGroups dataMaster) ([]), 
					stgdevices=(getStgDev dataMaster),
					filesys=(getFileSys dataMaster),
					volgroup=(getVolGr dataMaster),
					fileanddir=(getFileDir dataMaster)}	

updateGroupsDeleteUser :: String -> [Group] -> [Group] -> [Group]
updateGroupsDeleteUser user groups answer =
	if groups == [] then answer 
	else 
		updateGroupsDeleteUser (user) (tail groups) (answer++[Group {groupName=groupName(groups!!0), 
								groupID=groupID(groups!!0),
								associatedUsers=updateGroupsDeleteUserAux(user)(associatedUsers(groups!!0))([])}]) 

updateGroupsDeleteUserAux :: String -> [User] -> [User] -> [User]
updateGroupsDeleteUserAux name usersGroup answer =
	if usersGroup == [] then answer else
	if name == userName(usersGroup!!0) 
		then updateGroupsDeleteUserAux (name) (tail usersGroup) (answer)
		else updateGroupsDeleteUserAux (name) (tail usersGroup) (answer++[usersGroup!!0])

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
					                     , primaryGroup=searchGroup(command!!2)(getGroups(dataMaster))
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
					                     , primaryGroup=searchGroup(command!!2)(getGroups(dataMaster))
					                     , secundaryGroups=searchGroups(getGroupName(command!!4)("")([])) (getGroups(dataMaster)) ([])
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
	then( addUser dataMaster command ) 
		else do  
			putStrLn "Error parameters"
			menu dataMaster 

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
	if list == [] then str else
	do
		printList (tail list) (str++userName(list!!0)++", ")

printGroups :: [Group] -> DataMaster -> IO()
printGroups groups dataMaster =
	if groups == [] then menu dataMaster else
	do
		putStrLn(groupName(groups!!0)++"        "++show(groupID(groups!!0))++printList(associatedUsers(groups!!0))("     "))
		printGroups (tail groups) (dataMaster)

print2aryUserGroups :: [Group] -> String -> String
print2aryUserGroups groups answer =
	if groups == [] then answer 
		else
			print2aryUserGroups (tail groups) (answer++","++groupName(groups!!0))

printUsers :: [User] -> DataMaster -> IO()
printUsers users dataMaster =
	if users == [] then menu dataMaster else
	do
		putStrLn(userName(users!!0)++"      "++
					show(userID(users!!0))++"     "++
					groupName(primaryGroup(users!!0))++"     "++
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
		else findUser (tail users) (name)

printFingerUserGroups :: [Group] -> String -> String
printFingerUserGroups groups answer =
	if groups == [] then answer 
	else printFingerUserGroups (tail groups) (answer++groupName(groups!!0))

printFinger :: User -> DataMaster ->IO()
printFinger user dataMaster = 
	do 
		putStrLn ("UserName: "++(userName(user)))
		putStrLn ("UID: "++show(userID(user)))
		putStrLn ("HomeDirectory: "++(homeDirectory(user)))
		putStrLn ("Associated primary group: "++groupName(primaryGroup(user)))
		putStrLn ("Associated secondary groups: "++printFingerUserGroups(secundaryGroups(user))("") )
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

deleteUser :: String -> [User] -> [User] -> DataMaster -> IO()
deleteUser name users answer dataMaster =
	if users == [] 
		then do
			editDataMasterUserDelete (dataMaster) (name) (answer) 
	else if name == userName(users!!0) 
		then deleteUser(name)(tail users)(answer)(dataMaster) 
		else deleteUser(name)(tail users)(answer++[users!!0])(dataMaster)

userDel :: [String] -> DataMaster -> IO()
userDel command dataMaster =
	if(length(command) == 1 || length(command) > 2) 
		then do
				putStrLn "Error input userdel"
				menu dataMaster 
		else if(verifyUser(command!!1)) 
			then 
				deleteUser (command!!1) (getUsers dataMaster) ([]) (dataMaster)
			else do
				putStrLn "Error input userdel"
				menu dataMaster

groupDelete :: String -> [Group] -> [Group] -> DataMaster -> IO()
groupDelete group groups answer dataMaster =
	if groups == [] 
		then menu (DataMaster {users=(getUsers dataMaster), 
					groups=answer, 
					stgdevices=(getStgDev dataMaster),
					filesys=(getFileSys dataMaster),
					volgroup=(getVolGr dataMaster),
					fileanddir=(getFileDir dataMaster)})
		else
			if group == groupName(groups!!0)
				then groupDelete (group) (tail groups) (answer) dataMaster
				else groupDelete (group) (tail groups) (answer++[groups!!0]) dataMaster

groupDel :: [String] -> DataMaster -> IO()
groupDel command dataMaster =
	if(length(command) == 1 || length(command) > 2) 
		then do
		 		putStrLn "Error input groupdel"
		 		menu dataMaster
		else if(verifyGroup(command!!1)) 
			then 
				groupDelete (command!!1) (getGroups dataMaster) ([]) (dataMaster) 
			else do
				putStrLn "Error input groupdel"
		 		menu dataMaster

modifyUser4params :: String -> String -> [String] -> [User] -> DataMaster -> [User] -> [User]
modifyUser4params user primary secondary users dataMaster answer =
	if users == [] then answer else
	if user == userName(users!!0) 
		then modifyUser4params (user) (primary) (secondary) (tail users) (dataMaster) (answer++[User { userName=userName(users!!0)
													                     , userID=userID(users!!0)
													                     , primaryGroup=searchGroup (primary) (getGroups(dataMaster))
													                     , secundaryGroups=searchGroups(secondary)(getGroups(dataMaster))([])
													                     , homeDirectory=homeDirectory(users!!0)
													                     }])
		else modifyUser4params (user) (primary) (secondary) (tail users) (dataMaster) (answer++[users!!0])

modifyUser4paramsAux :: String -> String -> [String] -> [User] -> DataMaster -> [User] -> [User]
modifyUser4paramsAux user primary secondary users dataMaster answer =
	if users == [] then answer else
	if user == userName(users!!0) 
		then modifyUser4paramsAux (user) (primary) (secondary) (tail users) (dataMaster) (answer++[User { userName=userName(users!!0)
													                     , userID=userID(users!!0)
													                     , primaryGroup=primaryGroup(users!!0)
													                     , secundaryGroups=searchGroups(secondary)(getGroups(dataMaster))([])
													                     , homeDirectory=homeDirectory(users!!0)
													                     }])
		else modifyUser4paramsAux (user) (primary) (secondary) (tail users) (dataMaster) (answer++[users!!0])

modifyUserDataMaster :: [User] -> DataMaster -> IO()
modifyUserDataMaster chagedUsers dataMaster =
	menu (DataMaster {users=chagedUsers, 
					groups=(getGroups dataMaster), 
					stgdevices=(getStgDev dataMaster),
					filesys=(getFileSys dataMaster),
					volgroup=(getVolGr dataMaster),
					fileanddir=(getFileDir dataMaster)})

modUser4params :: [String] -> DataMaster -> IO()
modUser4params command dataMaster =
	if(command!!1 == "-g") 
		then modifyUserDataMaster (modifyUser4params (last command) (command!!2) ([]) (getUsers dataMaster) (dataMaster) ([])) (dataMaster)
	else if(command!!1 == "-G") 
		then modifyUserDataMaster (modifyUser4paramsAux (last command) ("") (getGroupName (command!!2) ("") ([])) (getUsers dataMaster) (dataMaster) ([])) (dataMaster)
		else do
			putStrLn "Error input params usermod"
			menu dataMaster

modifyUser6params :: String -> String -> [String] -> [User] -> DataMaster -> [User] -> [User]
modifyUser6params user primary secondary users dataMaster answer =
	if users == [] then answer else
	if user == userName(users!!0) 
		then modifyUser6params (user) (primary) (secondary) (tail users) (dataMaster) (answer++[User { userName=userName(users!!0)
													                     , userID=userID(users!!0)
													                     , primaryGroup=searchGroup (primary) (getGroups(dataMaster))
													                     , secundaryGroups=searchGroups(secondary)(getGroups(dataMaster))([])
													                     , homeDirectory=homeDirectory(users!!0)
													                     }])
		else modifyUser6params (user) (primary) (secondary) (tail users) (dataMaster) (answer++[users!!0])	
	

userModification :: [String] -> DataMaster -> IO()
userModification command dataMaster =
	if(length(command) == 2) 
	then do
		putStrLn "(usermod) Nothing to modify" 
		menu dataMaster 
	else if(length(command) == 4) 
		then if(verifyUser(last command)) 
			then modUser4params (command) (dataMaster) 
			else do 
				putStrLn "Error input usermod"
				menu dataMaster
	else if(length(command) == 6) 
		then if(verifyUser(last command)) 
			then modifyUserDataMaster (modifyUser6params (last command) (command!!2) (getGroupName (command!!4) ("") ([])) (getUsers dataMaster) (dataMaster) ([])) (dataMaster)
			else do
				putStrLn "Error input usermod"
				menu dataMaster
	else do
		putStrLn "Error input usermod"
		menu dataMaster

-- Users and Groups ###############################

-- Files and Directories ##########################

verifyDirsExist :: [String] -> Directory -> Bool
verifyDirsExist path fromDir =
	if length(path) == 1 then True
	else verifyGoesTo (path) (directories(fromDir))

verifyGoesTo :: [String] -> [Directory] -> Bool
verifyGoesTo name dirs =
	if dirs == [] then False else
	if name!!0 == nameDirectory(dirs!!0) 
		then verifyDirsExist (tail name) (dirs!!0)
		else verifyGoesTo (name) (tail dirs)

createPath :: String -> String -> [String] -> [String]
createPath path dir answer =
	if path == [] then answer else
	if (last path) == '/' 
		then createPath (init path) ("") ([dir]++answer)
		else createPath (init path) ([(last path)]++dir) (answer)

createDirectory :: [String] -> DataMaster -> IO()
createDirectory command dataMaster =
	if(command!!1 == "-p") 
		then do
			updateDirectoriesDataMaster (createDirectoryByForce (createPath(command!!2)("")([])) (command!!2) (getRootDirectory(dataMaster)) (dataMaster)) (dataMaster)
			menu dataMaster
	else if (head(command!!1) == '/') 
		then if((createPath(command!!1)("")([]))!!0 /= "" && length(createPath(command!!1)("")([])) == 1) 
			then newDirOnRoot ((createPath(command!!1)("")([]))!!0) (dataMaster)
			else if(verifyDirsExist (createPath(command!!1)("")([])) (getRootDirectory(dataMaster))) 
				then updateDirectoriesDataMaster (createDirectoryPath (createPath(command!!1)("")([])) (command!!1) (getRootDirectory(dataMaster)) (dataMaster)) (dataMaster)
				else do
					putStrLn "Error directory doesn't exist"
					menu dataMaster
		else do
			putStrLn "Error input mkdir"
			menu dataMaster

newDirOnRoot :: String -> DataMaster -> IO()
newDirOnRoot name dataMaster = 
	menu (DataMaster {users=(getUsers dataMaster), 
					groups=(getGroups dataMaster), 
					stgdevices=(getStgDev dataMaster),
					filesys=(getFileSys dataMaster),
					volgroup=(getVolGr dataMaster),
					fileanddir=(newDirOnRootAux(newDirectory(name)("/"))(dataMaster), newDirOnRootAux(newDirectory(name)("/"))(dataMaster))})

newDirOnRootAux :: Directory -> DataMaster -> Directory
newDirOnRootAux dirs dataMaster =
	Directory {nameDirectory="/"
	             , pathDirectory = "/"
	             , creationDateDirectory = "date"
	             , creationTimeDirectory = "time"
	             , userDirectory = "root"
	             , groupDirectory = "root"
	         	 , directories = ((directories(getRootDirectory(dataMaster)))++[dirs])
	         	 , files = files(getRootDirectory(dataMaster))
	         	 , symbolics = []}

createDirectoryinDirectory :: Directory -> String -> String -> Directory
createDirectoryinDirectory destiny name path =
	Directory {nameDirectory=nameDirectory(destiny)
	             , pathDirectory = pathDirectory(destiny)
	             , creationDateDirectory = creationDateDirectory(destiny)
	             , creationTimeDirectory = creationTimeDirectory(destiny)
	             , userDirectory = userDirectory(destiny)
	             , groupDirectory = groupDirectory(destiny)
	         	 , directories = (directories(destiny)++[newDirectory(name)(path)])
	         	 , files = files(destiny)
	         	 , symbolics = symbolics(destiny)}

createDirectoryPath :: [String] -> String -> Directory -> DataMaster -> Directory
createDirectoryPath path stringPath fromDir dataMaster =
	if length(path) == 1 
		then createDirectoryinDirectory (fromDir) (path!!0) (stringPath)
		else goesTo (path) (stringPath) (fromDir) (directories(fromDir)) ([]) (dataMaster)

goesTo :: [String] -> String -> Directory -> [Directory] -> [Directory] -> DataMaster -> Directory
goesTo name path directoryFather dirs answer dataMaster = 
	if (dirs == []) 
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
		else if nameDirectory(dirs!!0) == name!!0 
				then goesTo (name) (path) (directoryFather) (tail dirs) (answer++[createDirectoryPath (tail name) (path) (dirs!!0) (dataMaster)]) (dataMaster)
				else goesTo (name) (path) (directoryFather) (tail dirs) (answer++[dirs!!0]) (dataMaster)

createDirectoryByForce :: [String] -> String -> Directory -> DataMaster -> Directory
createDirectoryByForce path stringPath fromDir dataMaster =
	if length(path) == 1
		then createDirectoryinDirectory (fromDir) (path!!0) (stringPath)
		else goesToForce (path) (stringPath) (fromDir) (directories(fromDir)) ([]) (dataMaster)

goesToForce :: [String] -> String -> Directory -> [Directory] -> [Directory] -> DataMaster -> Directory
goesToForce name path directoryFather dirs answer dataMaster =
	if existDirectoryinDirectory (name!!0) (dirs) 
		then goesToForceAux (name) (path) (directoryFather) (dirs) (answer) (dataMaster)
		else goesToForce (name) (path) (createDirectoryinDirectory (directoryFather) (name!!0) ((pathDirectory(directoryFather))++(name!!0))) (directories(createDirectoryinDirectory (directoryFather) (name!!0) ((pathDirectory(directoryFather))++(name!!0)))) (answer) (dataMaster)

goesToForceAux :: [String] -> String -> Directory -> [Directory] -> [Directory] -> DataMaster -> Directory
goesToForceAux name path directoryFather dirs answer dataMaster =
	if (dirs == []) 
		then Directory {nameDirectory=nameDirectory(directoryFather)
             , pathDirectory = pathDirectory(directoryFather)
             , creationDateDirectory = creationDateDirectory(directoryFather)
             , creationTimeDirectory = creationTimeDirectory(directoryFather)
             , userDirectory = userDirectory(directoryFather)
             , groupDirectory = groupDirectory(directoryFather)
         	 , directories = answer
         	 , files = files(directoryFather)
         	 , symbolics = symbolics(directoryFather)} 
		else if(nameDirectory(dirs!!0) == name!!0) 
			then goesToForceAux (name) (path) (directoryFather) (tail dirs) (answer++[createDirectoryByForce (tail name) (path) (dirs!!0) (dataMaster)]) (dataMaster)
			else goesToForceAux (name) (path) (directoryFather) (tail dirs) (answer++[dirs!!0]) (dataMaster)

existDirectoryinDirectory :: String -> [Directory] -> Bool
existDirectoryinDirectory name dirs =
	if dirs == [] then False else
	if nameDirectory(dirs!!0) == name then True
		else existDirectoryinDirectory name (tail dirs)

updateDirectoriesDataMaster :: Directory -> DataMaster -> IO()
updateDirectoriesDataMaster dir dataMaster =
	menu (DataMaster {users=(getUsers dataMaster), 
					groups=(getGroups dataMaster), 
					stgdevices=(getStgDev dataMaster),
					filesys=(getFileSys dataMaster),
					volgroup=(getVolGr dataMaster),
					fileanddir=(dir, dir)})

updateDirectories :: [Directory] -> DataMaster -> IO()
updateDirectories dirs dataMaster =
	menu (DataMaster {users=(getUsers dataMaster), 
					groups=(getGroups dataMaster), 
					stgdevices=(getStgDev dataMaster),
					filesys=(getFileSys dataMaster),
					volgroup=(getVolGr dataMaster),
					fileanddir=(updateRootDirectory(dirs)(dataMaster), getActualPosition(dataMaster))})

updateRootDirectory :: [Directory] -> DataMaster -> Directory
updateRootDirectory dirs dataMaster =
	Directory {nameDirectory="/"
	             , pathDirectory = "/"
	             , creationDateDirectory = "date"
	             , creationTimeDirectory = "time"
	             , userDirectory = "root"
	             , groupDirectory = "root"
	         	 , directories = dirs
	         	 , files = []
	         	 , symbolics = []}

newDirectory :: String -> String -> Directory
newDirectory name path =
	Directory {nameDirectory=name
	             , pathDirectory = path
	             , creationDateDirectory = "date"
	             , creationTimeDirectory = "time"
	             , userDirectory = "root"
	             , groupDirectory = "root"
	         	 , directories = []
	         	 , files = []
	         	 , symbolics = []}

cdCommand :: [String] -> DataMaster -> IO()
cdCommand command dataMaster =
	if(length(command) == 2) 
		then if(command!!1 == "/") 
			then updateActualDirectory (getRootDirectory dataMaster) (dataMaster)
			else if(head (command!!1) == '/') 
				then cdCommandAux (createPath(command!!1)("")([])) (dataMaster)
				else do
				putStrLn "Error input cd command. (/)"
				menu dataMaster 
		else do
			putStrLn "Error input cd command."
			menu dataMaster
			

cdCommandAux :: [String] -> DataMaster -> IO()
cdCommandAux path dataMaster =
	if(length(path) == 1 && path!!0 == "") 
		then updateActualDirectory (getRootDirectory dataMaster) (dataMaster)
	else if(existDirectory (path) (getRootDirectory dataMaster)) 
		then advanceInDirectories (path) (directories(getRootDirectory dataMaster)) (dataMaster) (getRootDirectory dataMaster)
		else if(existLink(path) (getRootDirectory dataMaster)) 
			then advanceInLinkDir (path) (getRootDirectory dataMaster) (dataMaster)
			else do
				putStrLn "Directory doesn't exist."
				menu dataMaster

advanceInDirectories :: [String] -> [Directory] -> DataMaster -> Directory -> IO()
advanceInDirectories path dirs dataMaster answer =
	if path == [] then updateActualDirectory (answer) (dataMaster) else
	if path!!0 == nameDirectory(dirs!!0) 
		then advanceInDirectories (tail path) (directories(dirs!!0)) (dataMaster) (dirs!!0)
		else advanceInDirectories (path) (tail dirs) (dataMaster) (answer)

updateActualDirectory :: Directory -> DataMaster -> IO()
updateActualDirectory dir dataMaster =
	menu (DataMaster {users=(getUsers dataMaster), 
					groups=(getGroups dataMaster), 
					stgdevices=(getStgDev dataMaster),
					filesys=(getFileSys dataMaster),
					volgroup=(getVolGr dataMaster),
					fileanddir=(getRootDirectory dataMaster, dir)})

existLink :: [String] -> Directory -> Bool
existLink path dir =
	if length(path) == 1 then isLink (path!!0) (symbolics(dir))
	else existLinkAux (path) (directories(dir))

existLinkAux :: [String] -> [Directory] -> Bool
existLinkAux path dirs =
	if dirs == [] then False else
	if path!!0 == nameDirectory(dirs!!0)
		then existLink (tail path) (dirs!!0)
		else existLinkAux (path) (tail dirs)

isLink :: String -> [SymbolicLink] -> Bool
isLink name syms =
	if syms == [] then False else
	if name == nameSymbolic(syms!!0)
		then True
		else False 

advanceInLinkDir :: [String] -> Directory -> DataMaster -> IO()
advanceInLinkDir path dir dataMaster =
	cdCommandAux (goesToLink (path) (getRootDirectory dataMaster)) (dataMaster)

goesToLink :: [String] -> Directory -> [String]
goesToLink path dir =
	if length(path) == 1 then getPathLink (path!!0) (symbolics(dir))
	else goesToLinkAux (path) (directories(dir))

goesToLinkAux :: [String] -> [Directory] -> [String]
goesToLinkAux path dirs =
	if path!!0 == nameDirectory(dirs!!0)
		then goesToLink (tail path) (dirs!!0)
		else goesToLinkAux (path) (tail dirs) 

getPathLink :: String -> [SymbolicLink] -> [String]
getPathLink name syms = 
	if name == nameSymbolic(syms!!0) then (createPath (pathToLink(syms!!0)) ("") ([]))
	else getPathLink (name) (tail syms)

newFile :: String -> String -> File
newFile name path = File { nameFile = name
                     , content = ""
                     , pathFile = path
                     , creationDateFile = "date"
                     , creationTimeFile = "time"
                     , userFile = "root"
                     , groupFile = "root"
                     }

createFileinDirectory :: Directory -> String -> String -> Directory
createFileinDirectory destiny name path =
		Directory {nameDirectory=nameDirectory(destiny)
	             , pathDirectory = pathDirectory(destiny)
	             , creationDateDirectory = creationDateDirectory(destiny)
	             , creationTimeDirectory = creationTimeDirectory(destiny)
	             , userDirectory = userDirectory(destiny)
	             , groupDirectory = groupDirectory(destiny)
	         	 , directories = (directories(destiny))
	         	 , files = (files(destiny)++[newFile(name)(path)])
	         	 , symbolics = symbolics(destiny)}

touchCommand :: [String] -> DataMaster -> IO()
touchCommand command dataMaster =
	if(length(command) == 2)
		then if(head(command!!1) == '/')
			then if(verifyDirsExist (createPath(command!!1)("")([])) (getRootDirectory dataMaster)) 
				then updateDirectoriesDataMaster (createFile (createPath(command!!1)("")([])) (command!!1) (getRootDirectory(dataMaster)) (dataMaster)) (dataMaster) 
				else do
					putStrLn "Directory doesn't exist."
					menu dataMaster
			else updateDirectoriesDataMaster (createFile (createPath((pathDirectory(getActualPosition dataMaster))++command!!1)("")([])) ((pathDirectory(getActualPosition dataMaster))++command!!1) (getRootDirectory(dataMaster)) (dataMaster)) (dataMaster) 
 		else do
 			putStrLn "Error input touch."
 			menu dataMaster

createFile :: [String] -> String -> Directory -> DataMaster -> Directory
createFile path stringPath fromDir dataMaster =
	if(length(path) == 1)
		then createFileinDirectory (fromDir) (path!!0) (stringPath)
		else createFileAux (path) (stringPath) (fromDir) (directories(fromDir)) ([]) (dataMaster)

createFileAux :: [String] -> String -> Directory -> [Directory] -> [Directory] -> DataMaster -> Directory
createFileAux name path directoryFather dirs answer dataMaster =
	if dirs == []
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if nameDirectory(dirs!!0) == name!!0
	    	then createFileAux (name) (path) (directoryFather) (tail dirs) (answer++[createFile (tail name) (path) (dirs!!0) (dataMaster)]) (dataMaster)
	    	else createFileAux (name) (path) (directoryFather) (tail dirs) (answer++[dirs!!0]) (dataMaster)

fileExist :: [String] -> Directory -> Bool
fileExist path dir =
	if length(path) == 1 then isFileThere(path!!0) (files(dir))
	else
		fileExistAux (path) (dir) (directories(dir))

fileExistAux :: [String] -> Directory -> [Directory] -> Bool
fileExistAux name directoryFather dirs =
	if dirs == [] then False
	else if(name!!0 == nameDirectory(dirs!!0)) 
		then fileExist (tail name) (dirs!!0) 
		else fileExistAux (name) (directoryFather) (tail dirs)

isFileThere :: String -> [File] -> Bool
isFileThere name filesDir =
	if filesDir == [] then False
		else if(name == nameFile(filesDir!!0)) 
			then True 
			else False

verifyEcho :: [String] -> DataMaster -> Bool
verifyEcho command dataMaster =
	if(head(command!!1) == '"' && last(command!!1) == '"' && length(command!!1) > 2)
		then if((command!!2) == ">>" && fileExist(createPath(command!!3)("")([]))(getRootDirectory dataMaster))
			then True
			else False
		else False

echoCommand :: [String] -> DataMaster -> IO()
echoCommand command dataMaster =
	if length(command) == 4
		then if(verifyEcho(command)(dataMaster))
			then updateDirectoriesDataMaster (writeInFile (createPath(command!!3)("")([])) (command!!3) (command!!1) (getRootDirectory dataMaster) (dataMaster)) (dataMaster)
			else do
				putStrLn "Error in echo command or file doesn't exist"
				menu dataMaster
		else do
			putStrLn "Error input in command echo"
			menu dataMaster

writeInFile :: [String] -> String -> String -> Directory -> DataMaster -> Directory
writeInFile path stringPath info dir dataMaster =
	if length(path) == 1 
		then writeDataInFile (path!!0) (info) (files(dir)) (dir) ([])
		else writeInFileAux (path) (stringPath) (info) (dir) (directories(dir)) ([]) (dataMaster)

writeInFileAux :: [String] -> String -> String -> Directory -> [Directory] -> [Directory] -> DataMaster -> Directory
writeInFileAux name path info directoryFather dirs answer dataMaster =
	if dirs == []
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if name!!0 == nameDirectory(dirs!!0)
	    	then writeInFileAux (name) (path) (info) (directoryFather) (tail dirs) (answer++[writeInFile(tail name) (path) (info) (dirs!!0) (dataMaster)]) (dataMaster)
	    	else writeInFileAux (name) (path) (info) (directoryFather) (tail dirs) (answer++[dirs!!0]) (dataMaster)

writeDataInFile :: String -> String -> [File] -> Directory -> [File] -> Directory
writeDataInFile name info filesDir directoryFather answer =
	if filesDir == [] 
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = directories(directoryFather)
	         	 , files = answer
	         	 , symbolics = symbolics(directoryFather)}
	    else if (nameFile(filesDir!!0) == name)  
	    	then writeDataInFile (name) (info) (tail filesDir) (directoryFather) (answer++[writeDataInFileAux(info)(filesDir!!0)])
	    	else writeDataInFile (name) (info) (tail filesDir) (directoryFather) (answer++[filesDir!!0])
		
writeDataInFileAux :: String -> File -> File
writeDataInFileAux info file =
	File { nameFile = nameFile(file)
         , content = ((content(file))++['\n']++info)
         , pathFile = pathFile(file)
         , creationDateFile = creationDateFile(file)
         , creationTimeFile = creationTimeFile(file)
         , userFile = userFile(file)
         , groupFile = groupFile(file)}

rmdirCommand :: [String] -> DataMaster -> IO()
rmdirCommand command dataMaster =
	if length(command) == 2
		then if(existAndEmptyDirectory (createPath(command!!1)("")([])) (getRootDirectory dataMaster)) 
			then updateDirectoriesDataMaster (deleteDirectory(createPath(command!!1)("")([])) (getRootDirectory dataMaster) (dataMaster)) (dataMaster)
			else do
				putStrLn "Error removing directory. The directory ins't empty or not exist."
				menu dataMaster
		else do
			putStrLn "Error, file doesn't exist or path is incorrect."
			menu dataMaster

existAndEmptyDirectory :: [String] -> Directory -> Bool
existAndEmptyDirectory path dir =
	if length(path) == 1 then verifyEmptyDirectory (path!!0) (directories(dir))
	else
		existAndEmptyDirectoryAux (path) (dir) (directories(dir))

existAndEmptyDirectoryAux :: [String] -> Directory -> [Directory] -> Bool
existAndEmptyDirectoryAux path dir dirs =
	if dirs == [] then False
	else if(path!!0 == nameDirectory(dirs!!0)) 
		then existAndEmptyDirectory (tail path) (dirs!!0)
		else existAndEmptyDirectoryAux (path) (dir) (tail dirs)

verifyEmptyDirectory :: String -> [Directory] -> Bool
verifyEmptyDirectory name dirs =
	if dirs == [] then False else
	if name == nameDirectory(dirs!!0) 
		then if(directories(dirs!!0) == [] && files(dirs!!0) == []) 
			then True
			else False
		else verifyEmptyDirectory (name) (tail dirs)

deleteDirectory :: [String] -> Directory -> DataMaster -> Directory
deleteDirectory path dir dataMaster =
	if length(path) == 1 then deleteDirectoryCompletely (path!!0) (dir) (directories(dir)) ([])
	else
		deleteDirectoryAux (path) (dir) (directories(dir)) ([]) (dataMaster)

deleteDirectoryAux :: [String] -> Directory -> [Directory] -> [Directory] -> DataMaster -> Directory
deleteDirectoryAux path directoryFather dirs answer dataMaster =
	if dirs == []
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if(path!!0 == nameDirectory(dirs!!0)) 
	    	then deleteDirectoryAux (path) (directoryFather) (tail dirs) (answer++[deleteDirectory (tail path) (dirs!!0) (dataMaster)]) (dataMaster)
 	    	else deleteDirectoryAux (path) (directoryFather) (tail dirs) (answer++[dirs!!0]) (dataMaster)

deleteDirectoryCompletely :: String -> Directory -> [Directory] -> [Directory] -> Directory
deleteDirectoryCompletely name directoryFather dirs answer =
	if dirs == []
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if(name == nameDirectory(dirs!!0))
	    	then deleteDirectoryCompletely (name) (directoryFather) (tail dirs) (answer)
	    	else deleteDirectoryCompletely (name) (directoryFather) (tail dirs) (answer++[dirs!!0])

rmCommand :: [String] -> DataMaster -> IO()
rmCommand command dataMaster =
	if length(command) == 2
		then if (fileExist (createPath(command!!1) ("") ([])) (getRootDirectory dataMaster))
			then updateDirectoriesDataMaster (deleteFile (createPath(command!!1)("")([])) (getRootDirectory dataMaster) (dataMaster)) (dataMaster)
			else if(symbolicExist (createPath (command!!1) ("") ([])) (getRootDirectory dataMaster)) 
				then updateDirectoriesDataMaster (deleteSymbolic (createPath(command!!1)("")([])) (getRootDirectory dataMaster) (dataMaster)) (dataMaster)
				else do 
					putStrLn "File or symbolic link doesn't exist."
					menu dataMaster 
		else if(length(command) == 3) 
			then if((command!!1) == "-rf" || (command!!1) == "-fr") 
				then if(existDirectory (createPath(command!!2) ("") ([])) (getRootDirectory dataMaster)) 
					then updateDirectoriesDataMaster (deleteDirectory(createPath(command!!2)("")([])) (getRootDirectory dataMaster) (dataMaster)) (dataMaster)
					else do 
						putStrLn "Error deleting file, directory doesn't exist."
						menu dataMaster
				else do
					putStrLn "Error input rm command."
					menu dataMaster
			else do
				putStrLn "Error input rm command."
				menu dataMaster 

symbolicExist :: [String] -> Directory -> Bool
symbolicExist path directoryFather =
	if length(path) == 1 then lookForSymbolic (path!!0) (symbolics(directoryFather))
	else symbolicExistAux (path) (directoryFather) (directories(directoryFather))

symbolicExistAux :: [String] -> Directory -> [Directory] -> Bool
symbolicExistAux path directoryFather dirs =
	if dirs == [] then False else
	if path!!0 == nameDirectory(dirs!!0) then symbolicExist (tail path) (dirs!!0)
	else symbolicExistAux (path) (directoryFather) (tail dirs)

lookForSymbolic :: String -> [SymbolicLink] -> Bool
lookForSymbolic name dirs =
	if dirs == [] then False else
	if name == nameSymbolic(dirs!!0) then True 
	else lookForSymbolic (name) (tail dirs)

deleteSymbolic :: [String] -> Directory -> DataMaster -> Directory
deleteSymbolic path dir dataMaster =
	if length(path) == 1 then deleteSymbolicCompletely (path!!0) (dir) (symbolics(dir)) ([])
	else
		deleteSymbolicAux (path) (dir) (directories(dir)) ([]) (dataMaster)

deleteSymbolicAux :: [String] -> Directory -> [Directory] -> [Directory] -> DataMaster -> Directory
deleteSymbolicAux path directoryFather dirs answer dataMaster =
	if dirs == []
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if (path!!0 == nameDirectory(dirs!!0))
	    	then deleteSymbolicAux (path) (directoryFather) (tail dirs) (answer++[deleteSymbolic (tail path) (dirs!!0) (dataMaster)]) (dataMaster)
	    	else deleteSymbolicAux (path) (directoryFather) (tail dirs) (answer++[dirs!!0]) (dataMaster)

deleteSymbolicCompletely :: String -> Directory -> [SymbolicLink] -> [SymbolicLink] -> Directory
deleteSymbolicCompletely name directoryFather filesDir answer =
	if filesDir == [] 
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = directories(directoryFather)
	         	 , files = files(directoryFather)
	         	 , symbolics = answer}
	    else if (nameSymbolic(filesDir!!0) == name)
		    then deleteSymbolicCompletely (name) (directoryFather) (tail filesDir) (answer)
		    else deleteSymbolicCompletely (name) (directoryFather) (tail filesDir) (answer++[filesDir!!0])

existDirectory :: [String] -> Directory -> Bool
existDirectory path dir =
	if length(path) == 1 then existDirectoryinDirectory (path!!0) (directories(dir))
		else existDirectoryAux (path) (dir) (directories(dir))

existDirectoryAux :: [String] -> Directory -> [Directory] -> Bool
existDirectoryAux path directoryFather dirs =
	if dirs == [] then False else
	if path!!0 == nameDirectory(dirs!!0) then existDirectory (tail path) (dirs!!0)
	else existDirectoryAux (path) (directoryFather) (tail dirs) 

deleteFile :: [String] -> Directory -> DataMaster -> Directory
deleteFile path dir dataMaster =
	if length(path) == 1 then deleteFileCompletely (path!!0) (dir) (files(dir)) ([])
	else
		deleteFileAux (path) (dir) (directories(dir)) ([]) (dataMaster)

deleteFileAux :: [String] -> Directory -> [Directory] -> [Directory] -> DataMaster -> Directory
deleteFileAux path directoryFather dirs answer dataMaster =
	if dirs == []
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if (path!!0 == nameDirectory(dirs!!0))
	    	then deleteFileAux (path) (directoryFather) (tail dirs) (answer++[deleteFile (tail path) (dirs!!0) (dataMaster)]) (dataMaster)
	    	else deleteFileAux (path) (directoryFather) (tail dirs) (answer++[dirs!!0]) (dataMaster)

deleteFileCompletely :: String -> Directory -> [File] -> [File] -> Directory
deleteFileCompletely name directoryFather filesDir answer =
	if filesDir == [] 
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = directories(directoryFather)
	         	 , files = answer
	         	 , symbolics = symbolics(directoryFather)}
	    else if (nameFile(filesDir!!0) == name)
		    then deleteFileCompletely (name) (directoryFather) (tail filesDir) (answer)
		    else deleteFileCompletely (name) (directoryFather) (tail filesDir) (answer++[filesDir!!0])

chownCommand :: [String] -> DataMaster -> IO()
chownCommand command dataMaster =
	if length(command) == 3
		then if (existUserGroup (command!!1) (dataMaster))
			then if((fileExist (createPath (command!!2) ("") ([])) (getRootDirectory dataMaster)) || (existDirectory (createPath (command!!2) ("") ([])) (getRootDirectory dataMaster))) 
				then if(isADirectory (createPath (command!!2) ("") ([])) (getRootDirectory dataMaster)) 
					then updateDirectoriesDataMaster (changeUserGroupDir (createPath (command!!2) ("") ([])) (getUserGroup (command!!1) ("") ([]))  (getRootDirectory dataMaster)) (dataMaster)
					else updateDirectoriesDataMaster (changeUserGroupFile (createPath (command!!2) ("") ([])) (getUserGroup (command!!1) ("") ([])) (getRootDirectory dataMaster)) (dataMaster)
				else do
					putStrLn "Error input chown command"
					menu dataMaster 
			else do
				putStrLn "Error, group or user introduced doesn't exist."
				menu dataMaster
		else if length(command) == 4 
			then if(command!!1 == "-R")
				then if (existUserGroup (command!!2) (dataMaster))
					then if((existDirectory (createPath (command!!3) ("") ([])) (getRootDirectory dataMaster)))
						then if(isADirectory (createPath (command!!3) ("") ([])) (getRootDirectory dataMaster)) 
							then updateDirectoriesDataMaster (changeInfoForceDir (createPath (command!!3) ("") ([])) (getUserGroup (command!!2) ("") ([])) (getRootDirectory dataMaster)) (dataMaster)
							else do
								putStrLn "Error input chown command"
								menu dataMaster
						else do
							putStrLn "Error input chown command"
							menu dataMaster
					else do
						putStrLn "Error input chown command"
						menu dataMaster
				else do
					putStrLn "Error input chown command"
					menu dataMaster
			else do
				putStrLn "Error input chown command"
				menu dataMaster

isADirectory :: [String] -> Directory -> Bool
isADirectory path directoryFather =
	if length(path) == 1 then isDirThere (path!!0) (directories(directoryFather)) 
	else isADirectoryAux (path) (directoryFather) (directories(directoryFather))

isADirectoryAux :: [String] -> Directory -> [Directory] -> Bool
isADirectoryAux path directoryFather dirs =
	if path!!0 == nameDirectory(dirs!!0) then isADirectory (tail path) (dirs!!0)
	else isADirectoryAux (path) (directoryFather) (tail dirs)

isDirThere :: String -> [Directory] -> Bool
isDirThere name dirs =
	if dirs == [] then False else
	if name == nameDirectory(dirs!!0) then True
		else isDirThere (name) (tail dirs)

existUserGroup :: String -> DataMaster -> Bool
existUserGroup command dataMaster =
	if length(getUserGroup(command)("")([])) == 2
		then if existUser ((getUserGroup(command)("")([]))!!0) (getUsers(dataMaster))
			then if existGroup ((getUserGroup(command)("")([]))!!1) (getGroups(dataMaster))
				then True
				else False
			else False
		else False

existUser :: String -> [User] -> Bool
existUser name users =
	if users == [] then False else
	if name == userName(users!!0) then True
		else existUser (name) (tail users)

existGroup :: String -> [Group] -> Bool
existGroup name groups =
	if groups == [] then False else
	if name == groupName(groups!!0) then True
		else existGroup (name) (tail groups)

getUserGroup :: String -> String -> [String] -> [String]
getUserGroup command word answer =
	if command == "" then answer++[word] else
	if command!!0 == ':' then getUserGroup (tail command) ("") (answer++[word]) else
	getUserGroup (tail command) (word++[command!!0]) (answer)

changeUserGroupDir :: [String] -> [String] -> Directory -> Directory
changeUserGroupDir path info directoryFather =
	if length(path) == 1 then changeInfoDir (path!!0) (info) (directoryFather) (directories(directoryFather)) ([])
	else
		changeUserGroupDirAux (path) (info) (directoryFather) (directories(directoryFather)) ([])

changeUserGroupDirAux :: [String] -> [String] -> Directory -> [Directory] -> [Directory] -> Directory
changeUserGroupDirAux path info directoryFather dirs answer =
	if dirs == [] 
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if path!!0 == nameDirectory(dirs!!0) 
	    	then changeUserGroupDirAux (path) (info) (directoryFather) (tail dirs) (answer++[changeUserGroupDir (tail path) (info) (dirs!!0)])
	    	else changeUserGroupDirAux (path) (info) (directoryFather) (tail dirs) (answer++[dirs!!0]) 

changeInfoDir :: String -> [String] -> Directory -> [Directory] -> [Directory] -> Directory
changeInfoDir name info directoryFather dirs answer =
	if dirs == [] 
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if (name == nameDirectory(dirs!!0))
	    	then changeInfoDir (name) (info) (directoryFather) (tail dirs) (answer++[Directory {nameDirectory=nameDirectory(dirs!!0)
																				             , pathDirectory = pathDirectory(dirs!!0)
																				             , creationDateDirectory = creationDateDirectory(dirs!!0)
																				             , creationTimeDirectory = creationTimeDirectory(dirs!!0)
																				             , userDirectory = (info!!0)
																				             , groupDirectory = (info!!1)
																				         	 , directories = directories(dirs!!0)
																				         	 , files = files(dirs!!0)
																				         	 , symbolics = symbolics(dirs!!0)}])
	    	else changeInfoDir (name) (info) (directoryFather) (tail dirs) (answer++[dirs!!0])

changeUserGroupFile :: [String] -> [String] -> Directory -> Directory
changeUserGroupFile path info directoryFather =
	if length(path) == 1 then changeInfoFile (path!!0) (info) (directoryFather) (files(directoryFather)) ([])
	else
		changeUserGroupFileAux (path) (info) (directoryFather) (directories(directoryFather)) ([]) 

changeUserGroupFileAux :: [String] -> [String] -> Directory -> [Directory] -> [Directory] -> Directory
changeUserGroupFileAux path info directoryFather dirs answer =
	if dirs == [] 
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = answer
	         	 , files = files(directoryFather)
	         	 , symbolics = symbolics(directoryFather)}
	    else if path!!0 == nameDirectory(dirs!!0) 
	    	then changeUserGroupDirAux (path) (info) (directoryFather) (tail dirs) (answer++[changeUserGroupDir (tail path) (info) (dirs!!0)])
	    	else changeUserGroupDirAux (path) (info) (directoryFather) (tail dirs) (answer++[dirs!!0]) 

changeInfoFile :: String -> [String] -> Directory -> [File] -> [File] -> Directory
changeInfoFile name info directoryFather dirs answer =
	if dirs == [] 
		then Directory {nameDirectory=nameDirectory(directoryFather)
	             , pathDirectory = pathDirectory(directoryFather)
	             , creationDateDirectory = creationDateDirectory(directoryFather)
	             , creationTimeDirectory = creationTimeDirectory(directoryFather)
	             , userDirectory = userDirectory(directoryFather)
	             , groupDirectory = groupDirectory(directoryFather)
	         	 , directories = directories(directoryFather)
	         	 , files = answer
	         	 , symbolics = symbolics(directoryFather)}
	    else if name == nameFile(dirs!!0)
	    	then changeInfoFile (name) (info) (directoryFather) (tail dirs) (answer++[File { nameFile = nameFile(dirs!!0)
																				         , content = content(dirs!!0)
																				         , pathFile = pathFile(dirs!!0)
																				         , creationDateFile = creationDateFile(dirs!!0)
																				         , creationTimeFile = creationTimeFile(dirs!!0)
																				         , userFile = (info!!0)
																				         , groupFile = (info!!1)}])
	    	else changeInfoFile (name) (info) (directoryFather) (tail dirs) (answer++[dirs!!0])

changeInfoForceDir :: [String] -> [String] -> Directory -> Directory
changeInfoForceDir path info directoryFather =
	if path == [] 
		then changeFilesAndDirsUserGroupForce (info) (directoryFather)
		else changeInfoForceDirAux (path) (info) (directoryFather) (directories(directoryFather)) ([])

changeInfoForceDirAux :: [String] -> [String] -> Directory -> [Directory] -> [Directory] -> Directory
changeInfoForceDirAux path info directoryFather dirs answer =
	if dirs == [] 
		then Directory {nameDirectory=nameDirectory(directoryFather)
		             , pathDirectory = pathDirectory(directoryFather)
		             , creationDateDirectory = creationDateDirectory(directoryFather)
		             , creationTimeDirectory = creationTimeDirectory(directoryFather)
		             , userDirectory = userDirectory(directoryFather)
		             , groupDirectory = groupDirectory(directoryFather)
		         	 , directories = answer
		         	 , files = files(directoryFather)
		         	 , symbolics = symbolics(directoryFather)}
		else
			if(path!!0 == nameDirectory(dirs!!0)) 
				then changeInfoForceDirAux (path) (info) (directoryFather) (tail dirs) (answer++[changeInfoForceDir (tail path) (info) (dirs!!0)])   
				else changeInfoForceDirAux (path) (info) (directoryFather) (tail dirs) (answer++[dirs!!0])

changeFilesAndDirsUserGroupForce :: [String] -> Directory -> Directory
changeFilesAndDirsUserGroupForce info directoryFather =
	Directory {nameDirectory=nameDirectory(directoryFather)
             , pathDirectory = pathDirectory(directoryFather)
             , creationDateDirectory = creationDateDirectory(directoryFather)
             , creationTimeDirectory = creationTimeDirectory(directoryFather)
             , userDirectory = info!!0
             , groupDirectory = info!!1
         	 , directories = (updateDirectoriesUserGroup (info) (directories(directoryFather)) ([]))
         	 , files = (updateFilesUserGroup (info) (files(directoryFather)) ([]))
         	 , symbolics = symbolics(directoryFather)}

updateDirectoriesUserGroup :: [String] -> [Directory] -> [Directory] -> [Directory]
updateDirectoriesUserGroup info dirs answer =
	if dirs == [] then answer
		else updateDirectoriesUserGroup (info) (tail dirs) (answer++[Directory {nameDirectory=nameDirectory(dirs!!0)
																             , pathDirectory = pathDirectory(dirs!!0)
																             , creationDateDirectory = creationDateDirectory(dirs!!0)
																             , creationTimeDirectory = creationTimeDirectory(dirs!!0)
																             , userDirectory = (info!!0)
																             , groupDirectory = (info!!1)
																         	 , directories = directories(dirs!!0)
																         	 , files = files(dirs!!0)
																         	 , symbolics = symbolics(dirs!!0)}])

updateFilesUserGroup :: [String] -> [File] -> [File] -> [File]
updateFilesUserGroup info dirs answer =
	if dirs == [] then answer
		else updateFilesUserGroup (info) (tail dirs) (answer++[File { nameFile = nameFile(dirs!!0)
														         , content = content(dirs!!0)
														         , pathFile = pathFile(dirs!!0)
														         , creationDateFile = creationDateFile(dirs!!0)
														         , creationTimeFile = creationTimeFile(dirs!!0)
														         , userFile = (info!!0)
														         , groupFile = (info!!1)}])

lsCommand :: [String] -> DataMaster -> IO()
lsCommand command dataMaster =
	if (length(command) == 1) 
		then do
			lsPrintDirectories (directories(getActualPosition(dataMaster)))
			lsPrintFiles (files(getActualPosition(dataMaster)))
			lsPrintSymbolics (symbolics(getActualPosition(dataMaster)))
			putStrLn ""
			menu dataMaster
		else if(length(command) == 2) 
			then if(command!!1 == "/") 
				then do
					lsPrintDirectories (directories(getRootDirectory dataMaster))
					lsPrintFiles (files(getRootDirectory dataMaster))
					lsPrintSymbolics (symbolics(getRootDirectory dataMaster))
					putStrLn ""
					menu dataMaster
				else if(command!!1 == "-l")
						then do
							putStrLn "Error input ls command."
							menu dataMaster
						else if(isADirectory (createPath (command!!1) ("") ([])) (getRootDirectory dataMaster))
							then do 
								advanceToPrint (createPath (command!!1) ("") ([])) (getRootDirectory dataMaster)
								menu dataMaster
							else do
								putStrLn "Error input ls command"
								menu dataMaster
			else if(length(command) == 3)
				then if(command!!1 == "-l")
					then if(command!!2 == "/") 
						then do
							printRootDetails dataMaster
							menu dataMaster
						else if(isADirectory (createPath (command!!2) ("") ([])) (getRootDirectory dataMaster))
							then do
								advanceToPrintDetails (createPath (command!!2) ("") ([])) (getRootDirectory dataMaster)
								menu dataMaster
							else do
								putStrLn "Error input ls command"
								menu dataMaster
						else do
							putStrLn "Error input ls command"
							menu dataMaster
					else do
						putStrLn "Error input ls command"
						menu dataMaster

advanceToPrintDetails :: [String] -> Directory -> IO()
advanceToPrintDetails path dir =
	if length(path) == 1 then printDirectoryDataDetails (path!!0) (directories(dir)) else
	advanceToPrintDetailsAux (path) (dir) (directories(dir))

advanceToPrintDetailsAux :: [String] -> Directory -> [Directory] -> IO()
advanceToPrintDetailsAux path directoryFather dirs =
	if path!!0 == nameDirectory(dirs!!0) then advanceToPrintDetails (tail path) (dirs!!0)
		else advanceToPrintDetailsAux (path) (directoryFather) (tail dirs)

printDirectoryDataDetails :: String -> [Directory] -> IO()
printDirectoryDataDetails name dirs =
	if name == nameDirectory(dirs!!0) 
		then do
			lsPrintDetailsDir (directories(dirs!!0))
			lsPrintDetailsFiles (files(dirs!!0))
			lsPrintDetailsSymbolics (symbolics(dirs!!0))
			putStrLn ""
		else printDirectoryDataDetails (name) (tail dirs)

printRootDetails :: DataMaster -> IO()
printRootDetails dataMaster = do
	lsPrintDetailsDir (directories(getRootDirectory dataMaster))
	lsPrintDetailsFiles (files(getRootDirectory dataMaster))
	lsPrintDetailsSymbolics (symbolics(getRootDirectory dataMaster))
	putStrLn ""

lsPrintDetailsDir :: [Directory] -> IO()
lsPrintDetailsDir dirs =
	if (dirs == []) 
		then do
			putStr ""
	else do
		putStrLn ("d "++(userDirectory(dirs!!0))++":"++(groupDirectory(dirs!!0))++"    "++(creationDateDirectory(dirs!!0))++"    "++(creationTimeDirectory(dirs!!0))++"    "++(nameDirectory(dirs!!0)))
		lsPrintDetailsDir (tail dirs)

lsPrintDetailsFiles :: [File] -> IO()
lsPrintDetailsFiles dirs =
	if (dirs == []) 
		then do
			putStr ""
	else do
		putStrLn ("- "++(userFile(dirs!!0))++":"++(groupFile(dirs!!0))++"    "++(creationDateFile(dirs!!0))++"    "++(creationTimeFile(dirs!!0))++"    "++(nameFile(dirs!!0)))
		lsPrintDetailsFiles (tail dirs)

lsPrintDetailsSymbolics :: [SymbolicLink] -> IO()
lsPrintDetailsSymbolics dirs =
	if (dirs == []) 
		then do
			putStr ""
	else do
		putStrLn ("l "++(userSymbolic(dirs!!0))++":"++(groupSymbolic(dirs!!0))++"    "++(creationDateSymbolic(dirs!!0))++"    "++(creationTimeSymbolic(dirs!!0))++"    "++(nameSymbolic(dirs!!0))++" -> "++(pathToLink(dirs!!0)))
		lsPrintDetailsSymbolics (tail dirs)

advanceToPrint :: [String] -> Directory -> IO()
advanceToPrint path dir =
	if length(path) == 1 then printDirectoryData (path!!0) (directories(dir)) else
	advanceToPrintAux (path) (dir) (directories(dir))

advanceToPrintAux :: [String] -> Directory -> [Directory] -> IO()
advanceToPrintAux path directoryFather dirs =
	if path!!0 == nameDirectory(dirs!!0) then advanceToPrint (tail path) (dirs!!0)
		else advanceToPrintAux (path) (directoryFather) (tail dirs)

printDirectoryData :: String -> [Directory] -> IO()
printDirectoryData name dirs =
	if name == nameDirectory(dirs!!0) 
		then do
			lsPrintDirectories (directories(dirs!!0))
			lsPrintFiles (files(dirs!!0))
			lsPrintSymbolics (symbolics(dirs!!0))
			putStrLn ""
		else printDirectoryData (name) (tail dirs)

lsPrintDirectories :: [Directory] -> IO()
lsPrintDirectories dirs =
	if (dirs == []) 
		then do
			putStr ""
	else do
		putStr ((nameDirectory(dirs!!0))++" ")
		lsPrintDirectories (tail dirs)

lsPrintFiles :: [File] -> IO()
lsPrintFiles dirs =
	if (dirs == []) 
		then do
			putStr ""
	else do
		putStr ((nameFile(dirs!!0))++" ")
		lsPrintFiles (tail dirs)

lsPrintSymbolics :: [SymbolicLink] -> IO()
lsPrintSymbolics dirs =
	if (dirs == []) 
		then do
			putStr ""
	else do
		putStr ((nameSymbolic(dirs!!0))++" ")
		lsPrintSymbolics (tail dirs)

-- Files and Directories ##########################

-- Symbolic Links #################################

newSymbolinLink :: String -> String -> String -> SymbolicLink
newSymbolinLink name path pathToLink =
	SymbolicLink { nameSymbolic = name
                 , pathSymbolic = path
                 , pathToLink = pathToLink
                 , creationDateSymbolic = "date"
                 , creationTimeSymbolic = "time"
                 , userSymbolic = "root"
                 , groupSymbolic = "root" }	


lnCommand :: [String] -> DataMaster -> IO()
lnCommand command dataMaster =
	if length(command) == 4 
		then if(command!!1 == "-s") 
			then if(verifyDirsExist (createPath (command!!3) ("") ([])) (getRootDirectory dataMaster)) 
					then if((fileExist (createPath (command!!2) ("") ([])) (getRootDirectory dataMaster)) || (existDirectory (createPath (command!!2) ("") ([])) (getRootDirectory dataMaster)))
						then if(isADirectory (createPath (command!!2) ("") ([])) (getRootDirectory dataMaster))
							then updateDirectoriesDataMaster (createSymbolicLinkDir (createPath (command!!3) ("") ([])) (command!!3) (command!!2) (getRootDirectory dataMaster)) (dataMaster)
							else updateDirectoriesDataMaster (createSymbolicLinkFile (createPath (command!!3) ("") ([])) (command!!3) (command!!2) (getRootDirectory dataMaster)) (dataMaster)
					else do
						putStrLn "Error"
						menu dataMaster
				else do
					putStrLn "Error"
					menu dataMaster
			else do
				putStrLn "Error"
				menu dataMaster
		else do
			putStrLn "Error"
			menu dataMaster

createSymbolicLinkFile :: [String] -> String -> String -> Directory -> Directory
createSymbolicLinkFile path stringPath pathToLink dir =
	if length(path) == 1 then createSymbolic (path!!0) (stringPath) (pathToLink) (dir)
	else createSymbolicLinkFileAux (path) (stringPath) (pathToLink) (dir) (directories(dir)) ([]) 

createSymbolicLinkFileAux :: [String] -> String -> String -> Directory -> [Directory] -> [Directory] -> Directory
createSymbolicLinkFileAux path stringPath pathToLink directoryFather dirs answer =
	if dirs == []
		then Directory {nameDirectory=nameDirectory(directoryFather)
		             , pathDirectory = pathDirectory(directoryFather)
		             , creationDateDirectory = creationDateDirectory(directoryFather)
		             , creationTimeDirectory = creationTimeDirectory(directoryFather)
		             , userDirectory = userDirectory(directoryFather)
		             , groupDirectory = groupDirectory(directoryFather)
		         	 , directories = answer
		         	 , files = files(directoryFather)
		         	 , symbolics = symbolics(directoryFather)}
		else if path!!0 == nameDirectory(dirs!!0)
			then createSymbolicLinkFileAux (path) (stringPath) (pathToLink) (directoryFather) (tail dirs) (answer++[createSymbolicLinkFile (tail path) (stringPath) (pathToLink) (dirs!!0)])
			else createSymbolicLinkFileAux (path) (stringPath) (pathToLink) (directoryFather) (tail dirs) (answer++[dirs!!0]) 

createSymbolicLinkDir :: [String] -> String -> String -> Directory -> Directory
createSymbolicLinkDir path stringPath pathToLink dir =
	if length(path) == 1 then createSymbolic (path!!0) (stringPath) (pathToLink) (dir)
	else createSymbolicLinkDirAux (path) (stringPath) (pathToLink) (dir) (directories(dir)) ([]) 

createSymbolicLinkDirAux :: [String] -> String -> String -> Directory -> [Directory] -> [Directory] -> Directory
createSymbolicLinkDirAux path stringPath pathToLink directoryFather dirs answer =
	if dirs == []
		then Directory {nameDirectory=nameDirectory(directoryFather)
		             , pathDirectory = pathDirectory(directoryFather)
		             , creationDateDirectory = creationDateDirectory(directoryFather)
		             , creationTimeDirectory = creationTimeDirectory(directoryFather)
		             , userDirectory = userDirectory(directoryFather)
		             , groupDirectory = groupDirectory(directoryFather)
		         	 , directories = answer
		         	 , files = files(directoryFather)
		         	 , symbolics = symbolics(directoryFather)}
		else if path!!0 == nameDirectory(dirs!!0)
			then createSymbolicLinkDirAux (path) (stringPath) (pathToLink) (directoryFather) (tail dirs) (answer++[createSymbolicLinkDir (tail path) (stringPath) (pathToLink) (dirs!!0)])
			else createSymbolicLinkDirAux (path) (stringPath) (pathToLink) (directoryFather) (tail dirs) (answer++[dirs!!0]) 

createSymbolic :: String -> String -> String -> Directory -> Directory
createSymbolic name path pathToLink directoryFather =
	Directory {nameDirectory=nameDirectory(directoryFather)
             , pathDirectory = pathDirectory(directoryFather)
             , creationDateDirectory = creationDateDirectory(directoryFather)
             , creationTimeDirectory = creationTimeDirectory(directoryFather)
             , userDirectory = userDirectory(directoryFather)
             , groupDirectory = groupDirectory(directoryFather)
         	 , directories = directories(directoryFather)
         	 , files = files(directoryFather)
         	 , symbolics = symbolics(directoryFather)++[newSymbolinLink (name) (path) (pathToLink)]}

-- Symbolic Links #################################

main :: IO()
main = menuDefault DataMaster {users=[]
						, groups=[defaultRootGroup]
						, stgdevices=[]
						, filesys=[]
						, volgroup=[]
						, fileanddir=(defaultDirectory,defaultDirectory)}

-- Main ------------------------------------------
menu :: DataMaster -> IO()
menu dataMaster = do
	putStr ("# "++nameDirectory(snd(getFileDir(dataMaster)))++" >> ")
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
				userDel command dataMaster
		------------------------------------------
		else if (head(command) == "groupdel")
			then do
				groupDel command dataMaster
		------------------------------------------
		else if (head(command) == "usermod")
			then do
				userModification command dataMaster
		------------------------------------------
		else if (head(command) == "mkdir")
			then do
				createDirectory command dataMaster
		------------------------------------------
		else if (head(command) == "touch")
			then do
				touchCommand command dataMaster
		------------------------------------------
		else if (head(command) == "ls")
			then do
				lsCommand command dataMaster
		------------------------------------------
		else if (head(command) == "cd")
			then do
				cdCommand command dataMaster
		------------------------------------------
		else if (head(command) == "echo")
			then do
				echoCommand command dataMaster
		------------------------------------------
		else if (head(command) == "rmdir")
			then do
				rmdirCommand command dataMaster
		------------------------------------------
		else if (head(command) == "rm")
			then do
				rmCommand command dataMaster
		------------------------------------------
		else if (head(command) == "chown")
			then do
				chownCommand command dataMaster
		------------------------------------------
		else if (head(command) == "ln")
			then do
				lnCommand command dataMaster
		------------------------------------------
		------------------------------------------
		else do
		putStrLn $ "Error"
		menu dataMaster
	
-- Main -------------------------------------------