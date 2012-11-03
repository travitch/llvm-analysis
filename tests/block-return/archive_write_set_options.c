extern int a_fo(void);
extern int a_co(void);

int archive_write_set_options(void) {
  int r1, r2;

  r1 = a_fo();
  if(r1 < -20)
    return r1;

  r2 = a_co();
  if(r2 < -20)
    return r2;

  if(r1 == -20 && r2 == -20)
    return -20;

  return 0;
}
