#!/usr/bin/python
# coding=utf-8
import os, sys, time
import argparse

if __name__=="__main__":
    parser = argparse.ArgumentParser(description='Input parameters to this script')
    parser.add_argument("--tc_old", type=str, default="tc_sanity", help='the tc name which the new tc copy by ,default is tc_sanity')
    parser.add_argument("--tc_new", type=str, default="tc_By_GenTc", help='the new tc name ,default is tc_By_GenTc')
    parser.add_argument("--author", type=str, default="OpenAI_Codex", help='the author name, default is "OpenAI_Codex"')
    parser.add_argument("--tc_list", type=str, default="tc.f", help='the filelist which the new tc appended to, default is tc.f')
    parser.add_argument("--tc_pkg", type=str, default="tc_pkg.sv", help='the package which the new tc appended to, default is tc_pkg.sv')

    args = parser.parse_args()
    TcOldName = args.tc_old
    TcNewName = args.tc_new
    Author = args.author
    TcList = args.tc_list
    TcPkg = args.tc_pkg
    CurrTime = time.strftime("%Y-%m-%d",time.localtime())

    TcPath = sys.path[0]
    TcOld = os.path.abspath(os.path.join(TcPath,'src','{_TcOldName}.sv'.format(_TcOldName=TcOldName)))
    TcNew = os.path.abspath(os.path.join(TcPath,'src','{_TcNewName}.sv'.format(_TcNewName=TcNewName)))
    os.system('cp {_TcOld} {_TcNew}'.format(_TcOld=TcOld,_TcNew=TcNew))
    #替换头注释、宏声明、TC_NAME
    os.system('sed -i "s/\/\/File name    :.*/\/\/File name    : {_TcNewName}.sv/g" {_TcNew}'.format(_TcNewName=TcNewName,_TcNew=TcNew))
    os.system('sed -i "s/\/\/Author       :.*/\/\/Author       : {_Author}/g" {_TcNew}'.format(_Author=Author,_TcNew=TcNew))
    os.system('sed -i "s/\/\/Module name  :.*/\/\/Module name  : {_TcNewName}/g" {_TcNew}'.format(_TcNewName=TcNewName,_TcNew=TcNew))
    os.system('sed -i "s/\/\/Discribution :.*/\/\/Discribution : {_TcNewName}/g" {_TcNew}'.format(_TcNewName=TcNewName,_TcNew=TcNew))
    os.system('sed -i "s/\/\/Date         :.*/\/\/Date         : {_CurrTime}/g" {_TcNew}'.format(_CurrTime=CurrTime,_TcNew=TcNew))
    os.system('sed -i "s/\`ifndef.*_SV.*/\`ifndef {_UTcNewName}__SV/g" {_TcNew}'.format(_UTcNewName=TcNewName.upper(),_TcNew=TcNew))
    os.system('sed -i "s/\`define.*_SV.*/\`define {_UTcNewName}__SV/g" {_TcNew}'.format(_UTcNewName=TcNewName.upper(),_TcNew=TcNew))
    os.system('sed -i "s/\`define TC_NAME.*/\`define TC_NAME {_TcNewName}/g" {_TcNew}'.format(_TcNewName=TcNewName,_TcNew=TcNew))
    #添加file到filelist
    FileList = os.path.abspath(os.path.join(TcPath,'{_FileList}'.format(_FileList=TcList)))
    file = open(FileList,'a')
    file.write('// ./src/{_TcNewName}.sv\n'.format(_TcNewName=TcNewName))
    file.close
    #添加file到package
    PkgFile = os.path.abspath(os.path.join(TcPath,'{_PkgFile}'.format(_PkgFile=TcPkg)))
    newPkgFileLine = ""
    with open(PkgFile,"r") as file:
        for line in file:
            if line.replace(" ","").replace("\n","")=="endpackage":
                newPkgFileLine += '    `include "{_TcNewName}.sv"\n'.format(_TcNewName=TcNewName)
                newPkgFileLine += line
            else:
                newPkgFileLine += line
        file.close()
    file = open(PkgFile,'w')
    file.write(newPkgFileLine)
    file.close
