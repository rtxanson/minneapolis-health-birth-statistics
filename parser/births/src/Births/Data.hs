{-# LANGUAGE OverloadedStrings, KindSignatures #-}

module Births.Data (
   CrappyPdfToTextPage
 , NeighborhoodRecord(..)
 , ChunkedPage(..)
 , MotherDemo(..)
 , PregnancyStats(..)
 , header
) where

import Control.Applicative

import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V

type CrappyPdfToTextPage = T.Text

data PregnancyStats = PregnancyStats
  { gestationUnder37WksAmt        :: T.Text
  , gestationOver37WksAmt         :: T.Text

  , gestationUnder37WksPerc       :: T.Text
  , gestationOver37WksPerc        :: T.Text

  , birthWeightOver2500GramsAmt   :: T.Text
  , birthWeightUnder2500GramsAmt  :: T.Text

  , birthWeightOver2500GramsPerc  :: T.Text
  , birthWeightUnder2500GramsPerc :: T.Text

  , firstTrimesterCareBeganAmt    :: T.Text
  , secondTrimesterCareBeganAmt   :: T.Text
  , thirdTrimesterCareBeganAmt    :: T.Text
  , noCareAmt                     :: T.Text

  , firstTrimesterCareBeganPerc   :: T.Text
  , secondTrimesterCareBeganPerc  :: T.Text
  , thirdTrimesterCareBeganPerc   :: T.Text
  , noCarePerc                    :: T.Text

  , careAdequacyAdequateAmt       :: T.Text
  , careAdequacyIntermediateAmt   :: T.Text
  , careAdequacyInadequateAmt     :: T.Text
  , careAdequacyNoCareAmt         :: T.Text

  , careAdequacyAdequatePerc      :: T.Text
  , careAdequacyIntermediatePerc  :: T.Text
  , careAdequacyInadequatePerc    :: T.Text
  , careAdequacyNoCarePerc        :: T.Text
  } deriving (Show)

data MotherDemo = MotherDemo
  { whiteNonHispAmt       :: T.Text
  , blackNonHispAmt       :: T.Text
  , amerIndAmt            :: T.Text
  , asianPacificAmt       :: T.Text
  , hispanicAmt           :: T.Text

  , whiteNonHispPerc      :: T.Text
  , blackNonHispPerc      :: T.Text
  , amerIndAPerc          :: T.Text
  , asianPacificPerc      :: T.Text
  , hispanicPerc          :: T.Text

  , marriedAmt            :: T.Text
  , marriedPerc           :: T.Text
  , unmarriedAmt          :: T.Text
  , unmarriedPerc         :: T.Text

  , ageUnder18Amt         :: T.Text
  , ageUnder18Perc        :: T.Text
  , age18to19Amt          :: T.Text
  , age18to19Perc         :: T.Text
  , age20to34Amt          :: T.Text
  , age20to34Perc         :: T.Text
  , age35plusAmt          :: T.Text
  , age35plusPerc         :: T.Text

  , lessThanHighSchoolAmt    :: T.Text
  , lessThanHighSchoolPerc   :: T.Text
  , highSchoolGradAmt        :: T.Text
  , highSchoolGradPerc       :: T.Text
  , moreThanHighSchoolAmt    :: T.Text
  , moreThanHighSchoolPerc   :: T.Text
  } deriving (Show)

data NeighborhoodRecord = NeighborhoodRecord
  { nhoodName             :: T.Text
  , totalBirthCount       :: T.Text
  , totalBirthPerc        :: T.Text
  , motherStats           :: MotherDemo
  , pregnancyStats        :: PregnancyStats
  } deriving (Show)

nhood_header     =  [ ("neighborhood_name"                                ,     nhoodName)
                    , ("neighborhood_birth_count"                         ,     totalBirthCount)
                    , ("neighborhood_birth_percent"                       ,     totalBirthPerc)
                    ]

mother_header    =  [ ("mother_white_nonhisp_amt"                         ,     whiteNonHispAmt)
                    , ("mother_black_nonhisp_amt"                         ,     blackNonHispAmt)
                    , ("mother_amerind_amt"                               ,     amerIndAmt)
                    , ("mother_asian_pacific_islander_amt"                ,     asianPacificAmt)
                    , ("mother_hispanic_amt"                              ,     hispanicAmt)

                    , ("mother_white_nonhisp_perc"                        ,     whiteNonHispPerc)
                    , ("mother_black_nonhisp_perc"                        ,     blackNonHispPerc)
                    , ("mother_amerind_perc"                              ,     amerIndAPerc)
                    , ("mother_asian_pacific_islander_perc"               ,     asianPacificPerc)
                    , ("mother_hispanic_perc"                             ,     hispanicPerc)

                    , ("mother_married_amt"                               ,     marriedAmt)
                    , ("mother_married_perc"                              ,     marriedPerc)
                    , ("mother_unmarried_amt"                             ,     unmarriedAmt)
                    , ("mother_unmarried_perc"                            ,     unmarriedPerc)

                    , ("mother_age_under18_amt"                           ,     ageUnder18Amt)
                    , ("mother_age_under18_perc"                          ,     ageUnder18Perc)
                    , ("mother_age_18to19_amt"                            ,     age18to19Amt)
                    , ("mother_age_18to19_perc"                           ,     age18to19Perc)
                    , ("mother_age_20to34_amt"                            ,     age20to34Amt)
                    , ("mother_age_20to34_perc"                           ,     age20to34Perc)
                    , ("mother_age_35plus_amt"                            ,     age35plusAmt)
                    , ("mother_age_35plus_perc"                           ,     age35plusPerc)

                    , ("mother_education_less_than_high_school_amt"       ,     lessThanHighSchoolAmt)
                    , ("mother_education_less_than_high_school_perc"      ,     lessThanHighSchoolPerc)
                    , ("mother_education_high_school_grad_amt"            ,     highSchoolGradAmt)
                    , ("mother_education_high_school_grad_perc"           ,     highSchoolGradPerc)
                    , ("mother_education_more_than_high_school_amt"       ,     moreThanHighSchoolAmt)
                    , ("mother_education_more_than_high_school_perc"      ,     moreThanHighSchoolPerc)
                    ]

preg_header  =      [ ("pregnancy_gestation_under_37_wks_amt"             ,     gestationUnder37WksAmt)
                    , ("pregnancy_gestation_over_37_wks_amt"              ,     gestationOver37WksAmt)

                    , ("pregnancy_gestation_under_37_wks_perc"            ,     gestationUnder37WksPerc)
                    , ("pregnancy_gestation_over_37_wks_perc"             ,     gestationOver37WksPerc)

                    , ("pregnancy_birth_weight_over_2500_grams_amt"       ,     birthWeightOver2500GramsAmt)
                    , ("pregnancy_birth_weight_under_2500_grams_amt"      ,     birthWeightUnder2500GramsAmt)

                    , ("pregnancy_birth_weight_over_2500_grams_perc"      ,     birthWeightOver2500GramsPerc)
                    , ("pregnancy_birth_weight_under_2500_grams_perc"     ,     birthWeightUnder2500GramsPerc)

                    , ("pregnancy_first_trimester_care_began_amt"         ,     firstTrimesterCareBeganAmt)
                    , ("pregnancy_second_trimester_care_began_amt"        ,     secondTrimesterCareBeganAmt)
                    , ("pregnancy_third_trimester_care_began_amt"         ,     thirdTrimesterCareBeganAmt)
                    , ("pregnancy_no_care_amt"                            ,     noCareAmt)

                    , ("pregnancy_first_trimester_care_began_perc"        ,     firstTrimesterCareBeganPerc)
                    , ("pregnancy_second_trimester_care_began_perc"       ,     secondTrimesterCareBeganPerc)
                    , ("pregnancy_third_trimester_care_began_perc"        ,     thirdTrimesterCareBeganPerc)
                    , ("pregnancy_no_care_perc"                           ,     noCarePerc)

                    , ("pregnancy_care_adequacy_adequate_amt"             ,     careAdequacyAdequateAmt)
                    , ("pregnancy_care_adequacy_intermediate_amt"         ,     careAdequacyIntermediateAmt)
                    , ("pregnancy_care_adequacy_inadequate_amt"           ,     careAdequacyInadequateAmt)
                    , ("pregnancy_care_adequacy_no_care_amt"              ,     careAdequacyNoCareAmt)

                    , ("pregnancy_care_adequacy_adequate_perc"            ,     careAdequacyAdequatePerc)
                    , ("pregnancy_care_adequacy_intermediate_perc"        ,     careAdequacyIntermediatePerc)
                    , ("pregnancy_care_adequacy_inadequate_perc"          ,     careAdequacyInadequatePerc)
                    , ("pregnancy_care_adequacy_no_care_perc"             ,     careAdequacyNoCarePerc)

                    ]


header :: Header
header = V.fromList (nhood_fields ++ mother_fields ++ pregnancy_fields)
  where
    nhood_fields = fst <$> nhood_header
    mother_fields = fst <$> mother_header
    pregnancy_fields = fst <$> preg_header

instance ToNamedRecord NeighborhoodRecord where
     toNamedRecord neighborhood = namedRecord (nhood_fields ++ mother_fields ++ pregnancy_fields)

        where mother = motherStats neighborhood
              pregnancy = pregnancyStats neighborhood

              nhood_fields     = [(field_name .= toField (accessor neighborhood)) | (field_name, accessor) <- nhood_header]
              mother_fields    = [(field_name .= toField (accessor mother))       | (field_name, accessor) <- mother_header]
              pregnancy_fields = [(field_name .= toField (accessor pregnancy))    | (field_name, accessor) <- preg_header]

data ChunkedPage = ChunkedPage
  { tableHeader           :: [[T.Text]]
  , tableData             :: [[T.Text]]
  } deriving (Show)
