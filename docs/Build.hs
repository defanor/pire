-- {-# LANGUAGE OverloadedStrings #-}

-- -- {-# LANGUAGE RecordWildCards #-}

-- {-# LANGUAGE MultiWayIf #-}
 




import Development.Shake

-- import Development.Shake.Command

import Development.Shake.FilePath

-- import Development.Shake.Util


-- import Text.Regex.Posix


-- import Data.Maybe (fromMaybe)




main :: IO ()





main = shakeArgs shakeOptions {
  shakeFiles="."
  -- shakeFiles="_build/"        
  -- , shakeProgress=progressSimple
  } $ do


  
  phony "clean" $ do


    -- liftIO $ removeFiles "." [...]

    removeFilesAfter "." $ ["//*.dbk+", "//*.dbkxi"
                           , "//*.html"
                           , "//html"
                           , "//*.fo", "//*.pdf"
                           , "//pdf"
                           ]
    




  let dbks =
        tail [
          undefined
          , "intro.dbk"
          , "pi101.dbk"
          , "src+install.dbk"
          , "parsec.dbk"
          , "tour.dbk"
          ]



  -- need fonts as well
  "pdf" *> \lnk -> do

    let pdf = "pire.pdf"
    need [pdf]

    () <- cmd Shell $ tail [

      undefined
      
      , "ln -s -f "++pdf++" ./" ++ lnk
      ]
  
    return ()



  -- need fonts as well
  "*.pdf" *> \pdf -> do

    let fo = pdf -<.> "fo"
    need [fo]
    need ["cfg"]
    need ["fonts"]

    () <- cmd Shell $ tail [

      undefined
      
      , "fop -c cfg -fo "++fo++" -pdf "++pdf
      ]
  
    return ()





  "*.fo" *> \fo -> do

    let p = fo -<.> "dbk+"
    need [p]

    need ["fo.xsl"]

    () <- cmd Shell $ tail [

      undefined
      
      , "xsltproc --xinclude --xincludestyle --output "++fo++" --stringparam fop1.extensions 1 fo.xsl "++p
      ]
  
    return ()


  
  -- need fonts as well
  "html" *> \lnk -> do

    let html = "pire.html"
    need [html]

    () <- cmd Shell $ tail [

      undefined
      
      , "ln -s -f "++html++" ./" ++ lnk
      ]
  
    return ()




  "*.html" *> \h -> do

    let p = h -<.> "dbk+"

    -- let dbk = dropExtension tex
    need [p]

    () <- cmd Shell $ tail [

      undefined
      
      , "xsltproc --xinclude --xincludestyle --output "++h++" --stringparam use.extensions 0 ./html.xsl "++p
      ]
  
    return ()







  "*.dbk+" *> \p -> do

    -- turn off preprocessing, so that the html can be used for cut & paste
    -- still needed ?
    
    let xi = p -<.> "dbkxi"

    -- let dbk = dropExtension tex
    need [xi]

    () <- cmd Shell $ tail [

      undefined
      
      , "saxonb-xslt -xi -s:"++xi++" -xsl:preproc.xsl -o:"++p
      ]
  
    return ()


  

  "*.dbkxi" *> \xi -> do

    let dbk = xi -<.> "dbk"

    -- let dbk = dropExtension tex
    need [dbk]

    -- need all the dbk sources as well
    need dbks


    () <- cmd Shell $ tail [

      undefined

      -- want xincludes and just plain section tags...
      , "xmllint --xinclude "++dbk++" | saxonb-xslt -s:- -xsl:rm-xml-base-etc.xsl -o:"++xi
      ]
  
    return ()



  
  -- phony "fonts" $ do
  "fonts" *> \f -> do

    need $ tail  
      [
        undefined
      , "lucidatypewriter"
      , "palatino"
      , "palatinoi"
      , "optima"
      , "urwclassico"
      ]

    -- touch a file fonts as an indicator that the fonts
    -- have been created, 
    -- as opposed to using a phony target and creating them
    -- over and over again
  
    () <- cmd Shell $ tail [
      undefined
      , "touch "++f
      ]


    return ()


    
  phony "fclean" $ do

    need ["clean"]
    
    removeFilesAfter "." $ tail [
      undefined
      , "lucidatypewriter"
      , "palatino"
      , "palatinoi"
      , "urwclassico"
      , "optima"
      , "fonts"
      ]
    -- liftIO $ removeFiles "." [...]
  
  

  "lucidatypewriter" *> \f -> do

    () <- cmd Shell $ tail [

      undefined
      , "fop-ttfreader ~/myfonts/typewriter/ttf-lucida/LucidaTypewriterRegular.ttf "++f
      ]
  
    return ()


  "palatino" *> \f -> do

    () <- cmd Shell $ tail [

      undefined
      , "fop-ttfreader ~/myfonts/oldstyle/PalatinoLinotype.ttf "++f
      ]
  
    return ()



  "palatinoi" *> \f -> do

    () <- cmd Shell $ tail [

      undefined
      , "fop-ttfreader ~/myfonts/oldstyle/PalatinoLinotypeItalic.ttf "++f
      ]
    
    return ()

  
  "optima" *> \f -> do

    () <- cmd Shell $ tail [
      undefined
      , "fop-ttfreader ~/myfonts/sans-serif/optima/OptimumRoman.ttf "++f
      ]
    
    return ()



  "urwclassico" *> \f -> do

    () <- cmd Shell $ tail [

      undefined
      , "./my-fop-pfmreader ~/myfonts/sans-serif/urwclassico/fonts/pfm/urw/optima/uopr8a.pfm "++f
      ]
    
    return ()




{-
  remark: I often write lists as
  
    tail $ 
    [
      undefined
      , item1
      , item2
      , etc
      ]
  
  instead of just
  
    [
      item1
      , item2
      , etc
      ]
  
  that way I can more easily comment out items (lines), 
  or move them up/down, the first item
  is not in any way special
 -}
