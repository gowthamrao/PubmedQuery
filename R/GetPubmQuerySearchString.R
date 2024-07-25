#' Get PubMed Query Search String
#'
#' Constructs a search query string for PubMed based on specified search criteria. This function creates
#' a default search string using various epidemiological and cohort study related terms. It can be extended
#' to include specific affiliation filters, such as for Johnson & Johnson.
#'
#' @param meshTerm Character string specifying the main MeSH term to include in the search.
#' @param freeText (optional) Character string specifying free text to search in title and abstract.
#' @return A character string representing the complete PubMed search query.
#' @export
getPubmQuerySearchString <-
  function(meshTerm, freeText = NULL) {
    # Validate input types
    checkmate::assertString(meshTerm, null.ok = FALSE)
    
    # Define base query parts
    baseQuery <- paste0(
      "(",
      "(",
      "(retrospective cohort) OR (epidemiology[MeSH Terms]) OR (Epidemiologic Methods[MeSH Terms])",
      " OR (phenotype[Text Word]) OR (Validation Study[Publication Type])",
      " OR (positive predictive value[Text Word]) OR (Validation Studies as Topic[MeSH Terms])",
      " OR (Sensitivity and Specificity[MeSH Terms])",
      " OR (insurance OR claims OR administrative OR health care)",
      ") OR database OR algorithm",
      ") AND (",
      "(",
      "(Medicaid) OR (Medicare) OR (Truven) OR (Optum) OR (Medstat)",
      " OR (Nationwide Inpatient Sample) OR (National Inpatient Sample) OR (PharMetrics)",
      " OR (PHARMO) OR (ICD-9[Title/Abstract]) OR (ICD-10[Title/Abstract]) OR (IMS[Title/Abstract])",
      " OR (electronic medical record[Text Word]) OR (Denmark/epidemiology[MeSH Terms])",
      " OR (Veterans Affairs[Title/Abstract]) OR (Premier database[Title/Abstract])",
      " OR (Database Management System[MeSH Terms])",
      " OR (National Health Insurance Research [MeSH Terms])",
      " OR (administrative claims[Text Word]) OR (General Practice Research Database[Text Word])",
      " OR (Clinical Practice Research Datalink[Text Word])",
      " OR (The Health Improvement Network[Text Word])",
      ")",
      ")"
    )
    
    # Define excluded publication types
    removedUnwanted <- paste0(
      " NOT (",
      "(",
      "\"Clinical Trial\"[pt] OR \"Editorial\"[pt] OR \"Letter\"[pt]",
      " OR \"Randomized Controlled Trial\"[pt] OR \"Clinical Trial, Phase I\"[pt]",
      " OR \"Clinical Trial, Phase II\"[pt] OR \"Clinical Trial, Phase III\"[pt]",
      " OR \"Clinical Trial, Phase IV\"[pt] OR \"Comment\"[pt] OR \"Controlled Clinical Trial\"[pt]",
      " OR \"Case Reports\"[pt] OR \"Clinical Trials as Topic\"[Mesh] OR \"double-blind\"[All]",
      " OR \"placebo-controlled\"[All] OR \"pilot study\"[All] OR \"pilot projects\"[Mesh]",
      " OR \"Prospective Studies\"[Mesh] OR \"Genetics\"[Mesh] OR \"Genotype\"[Mesh]",
      " OR (biomarker[Title/Abstract])",
      ")",
      ")"
    )
    
    baseQuery <- paste0(baseQuery, removedUnwanted)
    
    # Construct query combining MeSH term and base query
    query <-
      paste0(meshTerm,
             "[MeSH Major Topic] OR ",
             meshTerm,
             "[Title/Abstract]")
    
    
    if (!is.null(freeText)) {
      freeTextQuery <- paste0(meshTerm, "[Title/Abstract]")
      query <- paste0("( ", query, " OR ", freeTextQuery, ")")
    }
    
    completeQuery <-
      paste0("( ", query, " ) AND ( ", baseQuery, ")")
    
    return(completeQuery)
  }
