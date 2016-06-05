import getpass
import sys
import telnetlib
import time
import thread
import random

lock = thread.allocate_lock()

names = ["aa  ", "bb  ", "cc  ", "dd  ", "ee  ", "ff  ", "gg  ", "hh  ", "ii  ", "jj  ", "kk  ", "ll  ", "mm  ",
"nn  ", "oo  ", "pp  ", "qq  ", "rr  ", "ss  ", "tt  ", "uu  ", "vv  ", "ww  ", "yy  ", "zz  "]

writes = ["uno", "dos", "sss", "qua", "cin", "las", "Fas", "Opa", "cas"]

cmds = ["LSD  ", "DEL  ", "CRE  ", "OPN  ", "WRT  ", "REA  ", "CLO  ", "BYE  "]

actual = []

fd = 0

#random.randint(1, 10) [1 - 10]

def printcmd(user, cmd):
    print "POR  Usuario " + str(user) + ": " + cmd
    return

def randcommand(user, tn):
    global fd
    global names
    global writes
    global cmds
    global actual
    global lock

    lenactual = 0
    if(actual != []):
        lenactual = len(actual) - 1

    i        = random.randint(0, 6)
    delvar   = random.randint(0, lenactual)
    crevar   = random.randint(0, len(names) - 1)
    wrtvarfd = random.randint(0, fd)
    wrtvar   = random.randint(0, len(writes) - 1)

    pluscmd = cmds[i]
    pluscmd = pluscmd[0:4]

    readsome = ""

    if (i == 0): #LSD
        printcmd(user, cmds[i])
        tn.write(cmds[i])
        sys.stdout.write("PARA usuario " + str(user) + ": " + tn.read_some())
        return 1

    elif (i == 1): #DEL
        if (actual != []):
            pluscmd = pluscmd + actual[delvar]
        else:
            pluscmd = pluscmd + names[wrtvar]

        printcmd(user, pluscmd)
        tn.write(pluscmd)
        sys.stdout.write("PARA usuario " + str(user) + ": " + tn.read_some())
        return 1

    elif (i == 2): #CRE
        pluscmd = pluscmd + names[crevar]
        printcmd(user, pluscmd)
        tn.write(pluscmd)
        readsome = tn.read_some()
        if (readsome == "OK"):
            lock.acquire()
            actual.append(names[crevar])
            lock.release()
        sys.stdout.write("PARA usuario " + str(user) + ": " + readsome)
        return 1

    elif (i == 3): #OPN
        pluscmd = pluscmd + names[crevar]
        printcmd(user, pluscmd)
        tn.write(pluscmd)
        readsome = tn.read_some()
        if (readsome[0:2] == "OK"):
            lock.acquire()
            fd = fd + 1
            lock.release()
        sys.stdout.write("PARA usuario " + str(user) + ": " + readsome)
        return 1

    elif (i == 4): #WRT
        pluscmd = pluscmd + "FD " + str(wrtvarfd) + " SIZE " + str(len(writes[wrtvar])) + " " + writes[wrtvar] + "  "
        printcmd(user, pluscmd)
        tn.write(pluscmd)
        sys.stdout.write("PARA usuario " + str(user) + ": " + tn.read_some())
        return 1

    elif (i == 5): #REA
        pluscmd = pluscmd + "FD " + str(wrtvarfd) + " SIZE " + str(random.randint(0, 3)) + "  "
        printcmd(user, pluscmd)
        tn.write(pluscmd)
        sys.stdout.write("PARA usuario " + str(user) + ": " + tn.read_some())
        return 1

    elif (i == 6): #CLO
        pluscmd = pluscmd + "FD " + str(wrtvarfd) + "  "
        printcmd(user, pluscmd)
        tn.write(pluscmd)
        sys.stdout.write("PARA usuario " + str(user) + ": " + tn.read_some())
        return 1

    elif (i == 7): #BYE
        printcmd(user, pluscmd)
        tn.write(pluscmd + " ")
        sys.stdout.write("PARA usuario " + str(user) + ": " + tn.read_some())
        return -1

    else:
        return -1

def main():
    NCONN = 4
    conn = []
    try:
        for i in range(0, NCONN):
            #print i
            time.sleep(1)
            conn.append(thread.start_new_thread(salame, ("Thread-"+str(i), [i])))
    except:
        print "ERROR: Unable to create new thread"

def salame(threadName, user):
    HOST = "127.0.0.1"
    PORT = 8001
    tn = telnetlib.Telnet(HOST, PORT)

    con = "CON  "

    time.sleep(1)

    randwait = random.randint(1,5)
    flag = 1

    print "Usuario " + str(user) + "hace CON"
    tn.write(con)
    sys.stdout.write("Para usuario: " + str(user) + tn.read_some())

    time.sleep(1)
    
    while(flag == 1):
        randwait = random.randint(1,5)
        time.sleep(randwait)
        flag = randcommand(user, tn)

    print "OKI DOKI FINISHED"
    tn.read_all()



if __name__ == "__main__":
    main()

while(1):
    pass