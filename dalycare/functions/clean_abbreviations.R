clean_abbreviations = function(string){
  #' @title
  #' clean_abbreviations
  #' @author
  #' christian brieghel
  #' @description
  #' Replaces commonly used Danish abbreviations (non-exhaustive list) containing punctuation to allow for better separation of free text into complete sentences.
  #' E.g. 'f.eks. ' to 'f_eks_' pattern. 
  #' @note  
  #' Caveat: time lapse with large datasets: subset data before use. 
  #' @example
  #' SP_Journalnotater_Del1 %>% mutate(notat_text = clean_abbreviations(notat_text))
  
  string = tolower({{string}})
  string = gsub(c(' afd\\.'), ' afd', string)
  string = gsub(c(' afs\\.'), ' afs', string)
  string = gsub(c(' alm\\.'), ' alm', string)
  string = gsub(c(' bla\\.'), ' bl_a', string)
  string = gsub(c(' bl\\.a\\.'), ' bl_a', string)
  string = gsub(c(' ca\\.'), ' cirka ', string)
  string = gsub(c(' cm\\.'), ' cm ', string)
  string = gsub(c(' dgl\\.'), ' dgl ', string)
  string = gsub(c(' dagl\\.'), ' dagl ', string)
  string = gsub(c(' dvs\\.'), ' dvs ', string)
  string = gsub(c(' etc\\.'), ' etc', string)
  string = gsub(c(' e\\.l\\.'), 'e_l', string)
  string = gsub(c(' evt\\.'), ' evt', string)
  string = gsub(c(' fx\\.'), ' fx', string)
  string = gsub(c(' f\\.eks\\.'), ' f_eks', string)
  string = gsub(c(' hø\\.'), ' højre ', string)
  string = gsub(c(' inj\\.'), ' inj ', string)
  string = gsub(c(' jf\\.'), ' jf', string)
  string = gsub(c(' inkl\\.'), ' inkl', string)
  string = gsub(c(' ifm\\.'), ' ifm', string)
  string = gsub(c(' kl\\.'), ' kl', string)
  string = gsub(c(' kg\\.'), ' kg', string)
  string = gsub(c(' m\\.h\\.p\\.'), ' mhp', string)
  string = gsub(c(' mg\\.'), ' mg', string)
  string = gsub(c(' mfl\\.'), ' mfl', string)
  string = gsub(c(' mhp\\.'), ' mhp', string)
  string = gsub(c(' mht\\.'), ' mht', string)
  string = gsub(c(' mv\\.'), ' mv', string)
  string = gsub(c(' md\\.'), ' md ', string)
  string = gsub(c(' mdr\\.'), ' mdr ', string)
  string = gsub(c(' mdl\\.'), ' mdl ', string)
  string = gsub(c(' osv\\.'), ' osv', string)
  string = gsub(c(' palp\\.'), ' palp', string)
  string = gsub(c(' pba\\.'), ' pba', string)
  string = gsub(c(' pt\\.'), ' pt ', string)
  string = gsub(c(' pga\\.'), ' pga', string)
  string = gsub(c(' p\\.g\\.a\\.'), ' pga', string)
  string = gsub(c(' port\\.'), ' port', string)
  string = gsub(c(' rtg\\.'), ' rtg ', string)
  string = gsub(c(' sv\\.t.| svt\\.| sv\\.t'), ' sv_t ', string)
  string = gsub(c(' tbl\\.'), ' tbl', string)
  string = gsub(c(' ugl\\.'), ' ugl', string)
  string = gsub(c(' ve\\.'), ' venstre', string)
  string = gsub(c(' vedr\\.'), ' vedr', string)
  string = gsub(c(' vha\\.'), ' vha', string)
  string = gsub(c(' v\\.h\\.a\\.'), ' vha', string)
  string = gsub(c(' rp\\.'), ' rp_', string)
  string = gsub(c(' dim\\.'), ' dim_', string)
  string = gsub(c(' cresc\\.'), ' cresc_', string)
  string = gsub(c(' cres\\.'), ' cresc_', string)
  string = gsub(c(' crec\\.'), ' cresc_', string)
}
