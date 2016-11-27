%%[8
#ifndef __BASE_UNICODE_H__
#define __BASE_UNICODE_H__
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unicode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
extern int u8_mbtouc (uint32_t *puc, const uint8_t *s, size_t n);
extern int u8_uctomb (uint8_t *s, uint32_t uc, int n);
%%]

%%[8
#endif // __BASE_UNICODE_H__
%%]
