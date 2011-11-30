#-------------------------------------------------------------------------------
MACRO (SET_GLOBAL_VARIABLE name value)
  SET (${name} ${value} CACHE INTERNAL "Used to pass variables between directories" FORCE)
ENDMACRO (SET_GLOBAL_VARIABLE)

#-------------------------------------------------------------------------------
MACRO (EXTERNAL_JPEG_LIBRARY compress_type jpeg_pic)
  # May need to build JPEG with PIC on x64 machines with gcc
  # Need to use CMAKE_ANSI_CFLAGS define so that compiler test works

  IF (${compress_type} MATCHES "SVN")
    EXTERNALPROJECT_ADD (JPEG
        SVN_REPOSITORY ${JPEG_URL}
        # [SVN_REVISION rev] 
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=OFF
            -DJPEG_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_ANSI_CFLAGS:STRING=${jpeg_pic}
    ) 
  ELSEIF (${compress_type} MATCHES "TGZ")
    EXTERNALPROJECT_ADD (JPEG
        URL ${JPEG_URL}
        URL_MD5 ""
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=OFF
            -DJPEG_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_ANSI_CFLAGS:STRING=${jpeg_pic}
    ) 
  ENDIF (${compress_type} MATCHES "SVN")
  EXTERNALPROJECT_GET_PROPERTY (JPEG BINARY_DIR SOURCE_DIR) 

  IF (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")
    IF (WIN32 AND NOT MINGW)
      SET (JPEG_LIB_NAME "jpeg_D")
    ELSE (WIN32 AND NOT MINGW)
      SET (JPEG_LIB_NAME "jpeg_debug")
    ENDIF (WIN32 AND NOT MINGW)
  ELSE (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")
    SET (JPEG_LIB_NAME "jpeg")
  ENDIF (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")

  # Create imported target szip
  ADD_LIBRARY(jpeg STATIC IMPORTED)
  ADD_DEPENDENCIES (jpeg JPEG)

  IF (${libtype} MATCHES "SHARED")
    IF (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(jpeg PROPERTIES
          IMPORTED_IMPLIB "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/${CMAKE_IMPORT_LIBRARY_PREFIX}${JPEG_LIB_NAME}${CMAKE_IMPORT_LIBRARY_SUFFIX}"
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/${CMAKE_IMPORT_LIBRARY_PREFIX}${JPEG_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
      )
    ELSE (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(jpeg PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_SHARED_LIBRARY_PREFIX}${JPEG_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
          IMPORTED_SONAME "${CMAKE_SHARED_LIBRARY_PREFIX}${JPEG_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}.2.1"
          SOVERSION "2.1"
      )
    ENDIF (WIN32 AND NOT MINGW)
  ELSE (${libtype} MATCHES "SHARED")
    IF (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(jpeg PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/lib${JPEG_LIB_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}"
          IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      )
    ELSE (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(jpeg PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/lib${JPEG_LIB_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}"
          IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      )
    ENDIF (WIN32 AND NOT MINGW)
  ENDIF (${libtype} MATCHES "SHARED")

#  INCLUDE (${BINARY_DIR}/JPEG-targets.cmake)  
  SET (JPEG_LIBRARY "jpeg")
  
  SET (JPEG_INCLUDE_DIR_GEN "${BINARY_DIR}")
  SET (JPEG_INCLUDE_DIR "${SOURCE_DIR}/src")
  SET (JPEG_FOUND 1)
  SET (JPEG_LIBRARIES ${JPEG_LIBRARY})
  SET (JPEG_INCLUDE_DIRS ${JPEG_INCLUDE_DIR_GEN} ${JPEG_INCLUDE_DIR})
ENDMACRO (EXTERNAL_JPEG_LIBRARY)

#-------------------------------------------------------------------------------
MACRO (PACKAGE_JPEG_LIBRARY compress_type)
  ADD_CUSTOM_TARGET (JPEG-GenHeader-Copy ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${JPEG_INCLUDE_DIR_GEN}/jconfig.h ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/
      COMMENT "Copying ${JPEG_INCLUDE_DIR_GEN}/jconfig.h to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/"
  )
  SET (EXTERNAL_HEADER_LIST ${EXTERNAL_HEADER_LIST} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/jconfig.h)
  IF (${compress_type} MATCHES "SVN" OR ${compress_type} MATCHES "TGZ")
    ADD_DEPENDENCIES (JPEG-GenHeader-Copy JPEG)
  ENDIF (${compress_type} MATCHES "SVN" OR ${compress_type} MATCHES "TGZ")
ENDMACRO (PACKAGE_JPEG_LIBRARY)

#-------------------------------------------------------------------------------
MACRO (EXTERNAL_SZIP_LIBRARY compress_type libtype encoding)
  IF (${compress_type} MATCHES "SVN")
    EXTERNALPROJECT_ADD (SZIP
        SVN_REPOSITORY ${SZIP_URL}
        # [SVN_REVISION rev] 
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
            -DSZIP_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_ANSI_CFLAGS:STRING=${CMAKE_ANSI_CFLAGS}
            -DSZIP_ENABLE_ENCODING:BOOL=${encoding}
    ) 
  ELSEIF (${compress_type} MATCHES "TGZ")
    EXTERNALPROJECT_ADD (SZIP
        URL ${SZIP_URL}
        URL_MD5 ""
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
            -DSZIP_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_ANSI_CFLAGS:STRING=${CMAKE_ANSI_CFLAGS}
            -DSZIP_ENABLE_ENCODING:BOOL=${encoding}
    ) 
  ENDIF (${compress_type} MATCHES "SVN")
  EXTERNALPROJECT_GET_PROPERTY (SZIP BINARY_DIR SOURCE_DIR) 

  IF (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")
    IF (WIN32 AND NOT MINGW)
      SET (SZIP_LIB_NAME "szip_D")
    ELSE (WIN32 AND NOT MINGW)
      SET (SZIP_LIB_NAME "szip_debug")
    ENDIF (WIN32 AND NOT MINGW)
  ELSE (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")
    SET (SZIP_LIB_NAME "szip")
  ENDIF (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")

  # Create imported target szip
  ADD_LIBRARY(szip ${libtype} IMPORTED)
  ADD_DEPENDENCIES (szip SZIP)

  IF (${libtype} MATCHES "SHARED")
    IF (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(szip PROPERTIES
          IMPORTED_IMPLIB "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/${CMAKE_IMPORT_LIBRARY_PREFIX}${SZIP_LIB_NAME}${CMAKE_IMPORT_LIBRARY_SUFFIX}"
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/${CMAKE_IMPORT_LIBRARY_PREFIX}${SZIP_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
      )
    ELSE (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(szip PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_SHARED_LIBRARY_PREFIX}${SZIP_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
          IMPORTED_SONAME "${CMAKE_SHARED_LIBRARY_PREFIX}${SZIP_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}.2.1"
          SOVERSION "2.1"
      )
    ENDIF (WIN32 AND NOT MINGW)
  ELSE (${libtype} MATCHES "SHARED")
    IF (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(szip PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/lib${SZIP_LIB_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}"
          IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      )
    ELSE (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(szip PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/lib${SZIP_LIB_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}"
          IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      )
    ENDIF (WIN32 AND NOT MINGW)
  ENDIF (${libtype} MATCHES "SHARED")

#  INCLUDE (${BINARY_DIR}/SZIP-targets.cmake)  
  SET (SZIP_LIBRARY "szip")

  SET (SZIP_INCLUDE_DIR_GEN "${BINARY_DIR}")
  SET (SZIP_INCLUDE_DIR "${SOURCE_DIR}/src")
  SET (SZIP_FOUND 1)
  SET (SZIP_LIBRARIES ${SZIP_LIBRARY})
  SET (SZIP_INCLUDE_DIRS ${SZIP_INCLUDE_DIR_GEN} ${SZIP_INCLUDE_DIR})
ENDMACRO (EXTERNAL_SZIP_LIBRARY)

#-------------------------------------------------------------------------------
MACRO (PACKAGE_SZIP_LIBRARY compress_type libtype)
  ADD_CUSTOM_TARGET (SZIP-GenHeader-Copy ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SZIP_INCLUDE_DIR_GEN}/SZconfig.h ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/
      COMMENT "Copying ${SZIP_INCLUDE_DIR_GEN}/SZconfig.h to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/"
  )
  SET (EXTERNAL_HEADER_LIST ${EXTERNAL_HEADER_LIST} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/SZconfig.h)
  IF (${compress_type} MATCHES "SVN" OR ${compress_type} MATCHES "TGZ")
    ADD_DEPENDENCIES (SZIP-GenHeader-Copy SZIP)
  ENDIF (${compress_type} MATCHES "SVN" OR ${compress_type} MATCHES "TGZ")
ENDMACRO (PACKAGE_SZIP_LIBRARY)

#-------------------------------------------------------------------------------
MACRO (EXTERNAL_ZLIB_LIBRARY compress_type libtype)
  IF (${compress_type} MATCHES "SVN")
    EXTERNALPROJECT_ADD (ZLIB
        SVN_REPOSITORY ${ZLIB_URL}
        # [SVN_REVISION rev] 
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
            -DZLIB_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_ANSI_CFLAGS:STRING=${CMAKE_ANSI_CFLAGS}
            -DHDF_LEGACY_NAMING:BOOL=${HDF_LEGACY_NAMING}
    ) 
  ELSEIF (${compress_type} MATCHES "TGZ")
    EXTERNALPROJECT_ADD (ZLIB
        URL ${ZLIB_URL}
        URL_MD5 ""
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
            -DZLIB_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_ANSI_CFLAGS:STRING=${CMAKE_ANSI_CFLAGS}
            -DHDF_LEGACY_NAMING:BOOL=${HDF_LEGACY_NAMING}
    ) 
  ENDIF (${compress_type} MATCHES "SVN")
  EXTERNALPROJECT_GET_PROPERTY (ZLIB BINARY_DIR SOURCE_DIR) 

  IF (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")
    IF (WIN32 AND NOT MINGW)
      SET (ZLIB_LIB_NAME "zlib_D")
    ELSE (WIN32 AND NOT MINGW)
      SET (ZLIB_LIB_NAME "z_debug")
    ENDIF (WIN32 AND NOT MINGW)
  ELSE (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")
    IF (WIN32 AND NOT MINGW)
      SET (ZLIB_LIB_NAME "zlib")
    ELSE (WIN32 AND NOT MINGW)
      SET (ZLIB_LIB_NAME "z")
    ENDIF (WIN32 AND NOT MINGW)
  ENDIF (${CMAKE_BUILD_TYPE} MATCHES "DEBUG")

  # Create imported target szip
  ADD_LIBRARY(zlib ${libtype} IMPORTED)
  ADD_DEPENDENCIES (zlib ZLIB)
  
  IF (${libtype} MATCHES "SHARED")
    IF (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(zlib PROPERTIES
          IMPORTED_IMPLIB "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/${CMAKE_IMPORT_LIBRARY_PREFIX}${ZLIB_LIB_NAME}${CMAKE_IMPORT_LIBRARY_SUFFIX}"
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/${CMAKE_IMPORT_LIBRARY_PREFIX}${ZLIB_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
      )
    ELSE (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(zlib PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_SHARED_LIBRARY_PREFIX}${ZLIB_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
          IMPORTED_SONAME "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_SHARED_LIBRARY_PREFIX}${ZLIB_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}.1.2"
          SOVERSION "1.2"
      )
    ENDIF (WIN32 AND NOT MINGW)
  ELSE (${libtype} MATCHES "SHARED")
    IF (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(zlib PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${BLDTYPE}/lib${ZLIB_LIB_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}"
          IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      )
    ELSE (WIN32 AND NOT MINGW)
      SET_TARGET_PROPERTIES(zlib PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/lib${ZLIB_LIB_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}"
          IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      )
    ENDIF (WIN32 AND NOT MINGW)
  ENDIF (${libtype} MATCHES "SHARED")

#  INCLUDE (${BINARY_DIR}/ZLIB-targets.cmake)  
  SET (ZLIB_LIBRARY "zlib")
  
  SET (ZLIB_INCLUDE_DIR_GEN "${BINARY_DIR}")
  SET (ZLIB_INCLUDE_DIR "${SOURCE_DIR}")
  SET (ZLIB_FOUND 1)
  SET (ZLIB_LIBRARIES ${ZLIB_LIBRARY})
  SET (ZLIB_INCLUDE_DIRS ${ZLIB_INCLUDE_DIR_GEN} ${ZLIB_INCLUDE_DIR})
ENDMACRO (EXTERNAL_ZLIB_LIBRARY)

#-------------------------------------------------------------------------------
MACRO (PACKAGE_ZLIB_LIBRARY compress_type libtype)
  ADD_CUSTOM_TARGET (ZLIB-GenHeader-Copy ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${ZLIB_INCLUDE_DIR_GEN}/zconf.h ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/
      COMMENT "Copying ${ZLIB_INCLUDE_DIR_GEN}/zconf.h to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/"
  )
  SET (EXTERNAL_HEADER_LIST ${EXTERNAL_HEADER_LIST} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/zconf.h)
  IF (${compress_type} MATCHES "SVN" OR ${compress_type} MATCHES "TGZ")
    ADD_DEPENDENCIES (ZLIB-GenHeader-Copy ZLIB)
  ENDIF (${compress_type} MATCHES "SVN" OR ${compress_type} MATCHES "TGZ")
ENDMACRO (PACKAGE_ZLIB_LIBRARY)

#-------------------------------------------------------------------------------
MACRO (IDE_GENERATED_PROPERTIES SOURCE_PATH HEADERS SOURCES)
  #set(source_group_path "Source/AIM/${NAME}")
  STRING (REPLACE "/" "\\\\" source_group_path ${SOURCE_PATH})
  source_group (${source_group_path} FILES ${HEADERS} ${SOURCES})

  #-- The following is needed if we ever start to use OS X Frameworks but only
  #--  works on CMake 2.6 and greater
  #SET_PROPERTY (SOURCE ${HEADERS}
  #       PROPERTY MACOSX_PACKAGE_LOCATION Headers/${NAME}
  #)
ENDMACRO (IDE_GENERATED_PROPERTIES)

#-------------------------------------------------------------------------------
MACRO (IDE_SOURCE_PROPERTIES SOURCE_PATH HEADERS SOURCES)
  #  INSTALL (FILES ${HEADERS}
  #       DESTINATION include/R3D/${NAME}
  #       COMPONENT Headers       
  #  )

  STRING (REPLACE "/" "\\\\" source_group_path ${SOURCE_PATH}  )
  source_group (${source_group_path} FILES ${HEADERS} ${SOURCES})

  #-- The following is needed if we ever start to use OS X Frameworks but only
  #--  works on CMake 2.6 and greater
  #SET_PROPERTY (SOURCE ${HEADERS}
  #       PROPERTY MACOSX_PACKAGE_LOCATION Headers/${NAME}
  #)
ENDMACRO (IDE_SOURCE_PROPERTIES)

#-------------------------------------------------------------------------------
MACRO (TARGET_NAMING target libtype)
  IF (WIN32 AND NOT MINGW)
    IF (${libtype} MATCHES "SHARED")
      IF (HDF_LEGACY_NAMING)
        SET_TARGET_PROPERTIES (${target} PROPERTIES OUTPUT_NAME "dll")
        SET_TARGET_PROPERTIES (${target} PROPERTIES PREFIX "${target}")
      ELSE (HDF_LEGACY_NAMING)
        SET_TARGET_PROPERTIES (${target} PROPERTIES OUTPUT_NAME "${target}dll")
      ENDIF (HDF_LEGACY_NAMING)
    ENDIF (${libtype} MATCHES "SHARED")
  ENDIF (WIN32 AND NOT MINGW)
ENDMACRO (TARGET_NAMING)

#-------------------------------------------------------------------------------
MACRO (HDF_SET_LIB_OPTIONS libtarget libname libtype)
  # message (STATUS "${libname} libtype: ${libtype}")
  IF (${libtype} MATCHES "SHARED")
    IF (WIN32 AND NOT MINGW)
      IF (HDF_LEGACY_NAMING)
        SET (LIB_RELEASE_NAME "${libname}dll")
        SET (LIB_DEBUG_NAME "${libname}ddll")
      ELSE (HDF_LEGACY_NAMING)
        SET (LIB_RELEASE_NAME "${libname}")
        SET (LIB_DEBUG_NAME "${libname}_D")
      ENDIF (HDF_LEGACY_NAMING)
    ELSE (WIN32 AND NOT MINGW)
      SET (LIB_RELEASE_NAME "${libname}")
      SET (LIB_DEBUG_NAME "${libname}_debug")
    ENDIF (WIN32 AND NOT MINGW)
  ELSE (${libtype} MATCHES "SHARED")
    IF (WIN32 AND NOT MINGW)
      IF (HDF_LEGACY_NAMING)
        SET (LIB_RELEASE_NAME "${libname}")
        SET (LIB_DEBUG_NAME "${libname}d")
      ELSE (HDF_LEGACY_NAMING)
        SET (LIB_RELEASE_NAME "lib${libname}")
        SET (LIB_DEBUG_NAME "lib${libname}_D")
      ENDIF (HDF_LEGACY_NAMING)
    ELSE (WIN32 AND NOT MINGW)
      # if the generator supports configuration types or if the CMAKE_BUILD_TYPE has a value
      IF (CMAKE_CONFIGURATION_TYPES OR CMAKE_BUILD_TYPE)
        SET (LIB_RELEASE_NAME "${libname}")
        SET (LIB_DEBUG_NAME "${libname}_debug")
      ELSE (CMAKE_CONFIGURATION_TYPES OR CMAKE_BUILD_TYPE)
        SET (LIB_RELEASE_NAME "lib${libname}")
        SET (LIB_DEBUG_NAME "lib${libname}_debug")
      ENDIF (CMAKE_CONFIGURATION_TYPES OR CMAKE_BUILD_TYPE)
    ENDIF (WIN32 AND NOT MINGW)
  ENDIF (${libtype} MATCHES "SHARED")
  
  SET_TARGET_PROPERTIES (${libtarget}
      PROPERTIES
      DEBUG_OUTPUT_NAME          ${LIB_DEBUG_NAME}
      RELEASE_OUTPUT_NAME        ${LIB_RELEASE_NAME}
      MINSIZEREL_OUTPUT_NAME     ${LIB_RELEASE_NAME}
      RELWITHDEBINFO_OUTPUT_NAME ${LIB_RELEASE_NAME}
  )
  
  #----- Use MSVC Naming conventions for Shared Libraries
  IF (MINGW AND ${libtype} MATCHES "SHARED")
    SET_TARGET_PROPERTIES (${libtarget}
        PROPERTIES
        IMPORT_SUFFIX ".lib"
        IMPORT_PREFIX ""
        PREFIX ""
    )
  ENDIF (MINGW AND ${libtype} MATCHES "SHARED")

ENDMACRO (HDF_SET_LIB_OPTIONS)

#-------------------------------------------------------------------------------
MACRO (TARGET_FORTRAN_WIN_PROPERTIES target addlinkflags)
  IF (WIN32 AND MSVC)
    IF (BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (${target}
          PROPERTIES
              COMPILE_FLAGS "/dll"
              LINK_FLAGS "/SUBSYSTEM:CONSOLE ${addlinkflags}"
      ) 
    ELSE (BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (${target}
          PROPERTIES
              COMPILE_FLAGS "/MD"
              LINK_FLAGS "/SUBSYSTEM:CONSOLE ${addlinkflags}"
      ) 
    ENDIF (BUILD_SHARED_LIBS)
  ENDIF (WIN32 AND MSVC)
ENDMACRO (TARGET_FORTRAN_WIN_PROPERTIES)
