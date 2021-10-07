#!/bin/sh

    #This program makes sure that the dynamic library can find gecode
    #adapt this to your gecode location

    #go to the appropriate file
    cd ..
    cd sources

    #update the path
    install_name_tool -change gecode.framework/Versions/49/gecode /Library/Frameworks/gecode.framework/Versions/49/gecode libgecode.dylib
