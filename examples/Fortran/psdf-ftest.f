C
C     psdf-ftest.f
C           
      program main
      real*8  t,m, r(3), v(3)
      integer  id, retval
      integer init_psdf_parser, read_body, write_body
      retval = init_psdf_parser()
 99   continue
      retval = read_body(id, t, m, r, v)
      retval = write_body(id, t, m, r, v)
      goto 99
      end
      
