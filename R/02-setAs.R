setAs("taxRateTable","data.frame",function(from){
  level <- from@level;
  start <- from@start;
  end <- from@end;
  taxRate <- from@taxRate;
  deductAmt <- from@deductAmt;
  res <- data.frame(level,start,end,taxRate,deductAmt,stringsAsFactors = FALSE);
  return(res)
})
