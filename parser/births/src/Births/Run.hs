{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Births.Run (
  runIt
 , splitPage
 , splitPages
 , pageToNhoodRecord
) where

import System.Environment 

import Control.Exception

import Births.Data 
import Births.Utils

import Control.Applicative
import Control.Monad

import Data.Csv (encode, encodeByName)
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Data.Text as T

(<~) = getRow

-- TODO:
-- community_set 

nhood_set :: [T.Text] -> (T.Text, T.Text)
nhood_set r = (r !! 0, r !! 1)

pageToNhoodRecord :: CrappyPdfToTextPage -> NeighborhoodRecord
pageToNhoodRecord p = NeighborhoodRecord { nhoodName = nhoodname
                                         , totalBirthCount = fst total_births
                                         , totalBirthPerc  = snd total_births
                                         , motherStats = mother
                                         , pregnancyStats = pregnancystats
                                         }
  where
    chunkedpage = splitPage p
    table_data = tableData chunkedpage
    header_data = tableHeader chunkedpage

    total_births = nhood_set $ table_data <~ "TOTAL BIRTHS"

    -- The neighborhood name is weird: it wraps over several lines, so, voodoo
    nhoodname = T.intercalate " " [okay | okay <- [(h !! 0) | h <- header_data]
                                        , T.unpack okay `notElem` ["NEIGHBORHOOD", "NUMBER"]
                                        ]

    row_mother_wht = nhood_set $ table_data <~ "WHITE NON-HISPANIC"
    row_mother_blk = nhood_set $ table_data <~ "BLACK NON-HISPANIC"
    row_mother_ami = nhood_set $ table_data <~ "AMERICAN INDIAN"
    row_mother_pcf = nhood_set $ table_data <~ "ASIAN/PACIFIC ISLANDER"
    row_mother_hsp = nhood_set $ table_data <~ "HISPANIC"

    row_married    = nhood_set $ table_data <~ "MARRIED"
    row_unmarried  = nhood_set $ table_data <~ "UNMARRIED"

    row_18         = nhood_set $ table_data <~ "UNDER 18"
    row_18_19      = nhood_set $ table_data <~ "18 - 19"
    row_20_34      = nhood_set $ table_data <~ "20 - 34"
    row_35_plus    = nhood_set $ table_data <~ "35 AND OVER"

    less_than_hs   = nhood_set $ table_data <~ "LESS THAN HIGH SCHOOL"
    hs_grad        = nhood_set $ table_data <~ "HIGH SCHOOL GRAD"
    more_than_hs   = nhood_set $ table_data <~ "MORE THAN HIGH SCHOOL"

    gestation_length_u_37     = nhood_set $ table_data <~ "UNDER 37 WEEKS"
    gestation_length_o_37     = nhood_set $ table_data <~ "37 OR MORE WEEKS"
    birth_weight_u_2500       = nhood_set $ table_data <~ "2500 + GRAMS"
    birth_weight_o_2500       = nhood_set $ table_data <~ "UNDER 2500 GRAMS"
    trimester_care_1st        = nhood_set $ table_data <~ "1st TRIMESTER"
    trimester_care_2nd        = nhood_set $ table_data <~ "2nd TRIMESTER"
    trimester_care_3rd        = nhood_set $ table_data <~ "3rd TRIMESTER"
    trimester_care_none       = nhood_set $ table_data <~ "NO CARE"
    adequacy_adequate         = nhood_set $ table_data <~ "ADEQUATE"
    adequacy_intermed         = nhood_set $ table_data <~ "INTERMEDIATE"
    adequacy_inadequate       = nhood_set $ table_data <~ "INADEQUATE"

    pregnancystats = PregnancyStats {
        gestationUnder37WksAmt        = fst gestation_length_u_37
      , gestationOver37WksAmt         = fst gestation_length_o_37

      , gestationUnder37WksPerc       = snd gestation_length_u_37
      , gestationOver37WksPerc        = snd gestation_length_o_37

      , birthWeightOver2500GramsAmt   = fst birth_weight_u_2500
      , birthWeightUnder2500GramsAmt  = fst birth_weight_o_2500

      , birthWeightOver2500GramsPerc  = snd birth_weight_u_2500
      , birthWeightUnder2500GramsPerc = snd birth_weight_o_2500

      , firstTrimesterCareBeganAmt    = fst trimester_care_1st
      , secondTrimesterCareBeganAmt   = fst trimester_care_2nd
      , thirdTrimesterCareBeganAmt    = fst trimester_care_3rd
      , noCareAmt                     = fst trimester_care_none

      , firstTrimesterCareBeganPerc   = snd trimester_care_1st
      , secondTrimesterCareBeganPerc  = snd trimester_care_2nd
      , thirdTrimesterCareBeganPerc   = snd trimester_care_3rd
      , noCarePerc                    = snd trimester_care_none

      , careAdequacyAdequateAmt       = fst adequacy_adequate
      , careAdequacyIntermediateAmt   = fst adequacy_intermed
      , careAdequacyInadequateAmt     = fst adequacy_inadequate
      , careAdequacyNoCareAmt         = fst trimester_care_none

      , careAdequacyAdequatePerc      = snd adequacy_adequate
      , careAdequacyIntermediatePerc  = snd adequacy_intermed
      , careAdequacyInadequatePerc    = snd adequacy_inadequate
      , careAdequacyNoCarePerc        = snd trimester_care_none

    }

    mother = MotherDemo {
        whiteNonHispAmt     =  fst row_mother_wht
      , blackNonHispAmt     =  fst row_mother_blk
      , amerIndAmt          =  fst row_mother_ami
      , asianPacificAmt     =  fst row_mother_pcf
      , hispanicAmt         =  fst row_mother_hsp

      , whiteNonHispPerc    =  snd row_mother_wht
      , blackNonHispPerc    =  snd row_mother_blk
      , amerIndAPerc        =  snd row_mother_ami
      , asianPacificPerc    =  snd row_mother_pcf
      , hispanicPerc        =  snd row_mother_hsp

      , marriedAmt          =  fst row_married
      , marriedPerc         =  snd row_married
      , unmarriedAmt        =  fst row_unmarried
      , unmarriedPerc       =  snd row_unmarried

      , ageUnder18Amt          =  fst row_18
      , ageUnder18Perc         =  snd row_18
      , age18to19Amt           =  fst row_18_19
      , age18to19Perc          =  snd row_18_19
      , age20to34Amt           =  fst row_20_34
      , age20to34Perc          =  snd row_20_34
      , age35plusAmt           =  fst row_35_plus
      , age35plusPerc          =  snd row_35_plus

      , lessThanHighSchoolAmt    =  fst less_than_hs
      , lessThanHighSchoolPerc   =  snd less_than_hs
      , highSchoolGradAmt        =  fst hs_grad
      , highSchoolGradPerc       =  snd hs_grad
      , moreThanHighSchoolAmt    =  fst more_than_hs
      , moreThanHighSchoolPerc   =  snd more_than_hs

    }

-- | Read file, split and filter pages, parse to records, return CSV
fileToCsv :: String -> IO BS.ByteString
fileToCsv fname = do
    ls <- readFile fname

    let recs = pageToNhoodRecord <$> (filterPages $ splitPages (T.pack ls))

    return $ encodeByName header recs
    

-- | Get filename arg
runIt :: IO ()
runIt = do
    args <- getArgs
    let fname = if length args > 0 then args !! 0
                                   else "minneapolis/2009-2011.txt"

    csv <- fileToCsv fname
    BS.putStrLn csv
