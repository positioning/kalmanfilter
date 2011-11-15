void verify_identifier_string(const char* str1)
{
#if defined(CHK_ID_STRING)
  // Back up the stream and read the number of bytes written in the
  // ``write function'' corresponding to this ``read function''
  long int num_bytes=strlen(str1);
  char str[10];
  str[num_bytes]='\0';
  gradient_structure::get_fp()->fread(str,num_bytes);
  //clogf << "in verify_id_string " << str1 << endl;
  if(strcmp(str1,str))
  {
    cerr << "Error reading stack identifer for " << str1 << endl;
    ad_exit(1);
  }
#endif
}



int save_identifier_string(const char* str)
{
#if defined(CHK_ID_STRING)
  int wsize=sizeof(char);
  int length=strlen(str);
  gradient_structure::get_fp()->fwrite(str,length);
#endif
  return 0;
}

