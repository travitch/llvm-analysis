extern int find_newc_header(void);
extern void* __archive_read_ahead(void);
extern void __archive_read_consume(void);
extern int rand(void);

extern int g;

int header_newc(void) {
  int r;
  void *h;

  r = find_newc_header();
  if(r < 100)
    return r;

  h = __archive_read_ahead();
  if(h == 0)
    return -10;

  __archive_read_consume();

  if(rand())
    g = 0;
  else if(rand())
    g = 1001;
  else {}

  __archive_read_consume();

  return r;
}
