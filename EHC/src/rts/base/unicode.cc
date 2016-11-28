%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unicode, taken from gnulib (sorry had no time to deal with proper GPL inclusion etc ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
#include "../rts.h"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unicode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
// copied/adapted from: http://ab-initio.mit.edu/octave-Faddeeva/gnulib/lib/unistr/u8-mbtouc.c
// there seems to be no standard lib for this

int
u8_mbtouc (uint32_t *puc, const uint8_t *s, size_t n)
{
  uint8_t c = *s;

  if (c < 0x80)
    {
      *puc = c;
      return 1;
    }
  else if (c >= 0xc2)
    {
      if (c < 0xe0)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40)
                {
                  *puc = ((unsigned int) (c & 0x1f) << 6)
                         | (unsigned int) (s[1] ^ 0x80);
                  return 2;
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return 1;
            }
        }
      else if (c < 0xf0)
        {
          if (n >= 3)
            {
              if ((s[1] ^ 0x80) < 0x40)
                {
                  if ((s[2] ^ 0x80) < 0x40)
                    {
                      if ((c >= 0xe1 || s[1] >= 0xa0)
                          && (c != 0xed || s[1] < 0xa0))
                        {
                          *puc = ((unsigned int) (c & 0x0f) << 12)
                                 | ((unsigned int) (s[1] ^ 0x80) << 6)
                                 | (unsigned int) (s[2] ^ 0x80);
                          return 3;
                        }
                      /* invalid multibyte character */
                      *puc = 0xfffd;
                      return 3;
                    }
                  /* invalid multibyte character */
                  *puc = 0xfffd;
                  return 2;
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              if (n == 1 || (s[1] ^ 0x80) >= 0x40)
                return 1;
              else
                return 2;
            }
        }
      else if (c < 0xf8)
        {
          if (n >= 4)
            {
              if ((s[1] ^ 0x80) < 0x40)
                {
                  if ((s[2] ^ 0x80) < 0x40)
                    {
                      if ((s[3] ^ 0x80) < 0x40)
                        {
                          if ((c >= 0xf1 || s[1] >= 0x90)
                              && (c < 0xf4 || (c == 0xf4 && s[1] < 0x90))
                             )
                            {
                              *puc = ((unsigned int) (c & 0x07) << 18)
                                     | ((unsigned int) (s[1] ^ 0x80) << 12)
                                     | ((unsigned int) (s[2] ^ 0x80) << 6)
                                     | (unsigned int) (s[3] ^ 0x80);
                              return 4;
                            }
                          /* invalid multibyte character */
                          *puc = 0xfffd;
                          return 4;
                        }
                      /* invalid multibyte character */
                      *puc = 0xfffd;
                      return 3;
                    }
                  /* invalid multibyte character */
                  *puc = 0xfffd;
                  return 2;
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              if (n == 1 || (s[1] ^ 0x80) >= 0x40)
                return 1;
              else if (n == 2 || (s[2] ^ 0x80) >= 0x40)
                return 2;
              else
                return 3;
            }
        }
    }
  /* invalid multibyte character */
  *puc = 0xfffd;
  return 1;
}

int
u8_uctomb (uint8_t *s, uint32_t uc, int n)
{
  if (uc < 0x80)
    {
      if (n > 0)
        {
          s[0] = uc;
          return 1;
        }
      /* else return -2, below.  */
    }
  else
    {
      int count;

      if (uc < 0x800)
        count = 2;
      else if (uc < 0x10000)
        {
          if (uc < 0xd800 || uc >= 0xe000)
            count = 3;
          else
            return -1;
        }
#if 0
      else if (uc < 0x200000)
        count = 4;
      else if (uc < 0x4000000)
        count = 5;
      else if (uc <= 0x7fffffff)
        count = 6;
#else
      else if (uc < 0x110000)
        count = 4;
#endif
      else
        return -1;

      if (n >= count)
        {
          switch (count) /* note: code falls through cases! */
            {
#if 0
            case 6: s[5] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0x4000000;
            case 5: s[4] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0x200000;
#endif
            case 4: s[3] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0x10000;
            case 3: s[2] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0x800;
            case 2: s[1] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0xc0;
          /*case 1:*/ s[0] = uc;
            }
          return count;
        }
    }
  return -2;
}



%%]

