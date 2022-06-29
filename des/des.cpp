#include <iostream>
#include <string>
#include "utils.hpp"

using namespace std;

const int n_rounds = 16;

class DES
{
/* 
64-bit plain text handed over to an initial permutation - IP - function
IP produces 2 halves of the permuted block - LPT and RPT
each _PT go through 16 (n_rounds) rounds of the encryption process
LPT and RPT are rejoined
final permutation - FP - is performed over the combined block
result - 64-bit cyphertext
*/
  public:
  DES(string key) {_key = hex_to_bin(key); generate_keys();}
  string encrypt(string plaintext); 

  private:
  string _encrypt(string pt);
  /* keys and all about them */
  string _key;
  string _keys[n_rounds];
  
  /* keys are shifted 1 or 2 bits depending on round */
  const int key_shift[n_rounds] = { 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1 };
  /* compression permutation to reduce 64-bit key to a 56-bit key */
  const int pc_1[56] = { 57, 49, 41, 33, 25, 17,  9, 
	                      1, 58, 50, 42, 34, 26, 18, 
                    	 10,  2, 59, 51, 43, 35, 27, 
                    	 19, 11,  3, 60, 52, 44, 36,		 
	                     63, 55, 47, 39, 31, 23, 15, 
	                      7, 62, 54, 46, 38, 30, 22, 
	                     14,  6, 61, 53, 45, 37, 29, 
	                     21, 13,  5, 28, 20, 12,  4 };
  /* compression permutation to reduce 56-bit key to a 48-bit key */
  const int pc_2[48] = { 14, 17, 11, 24,  1,  5,
				          3, 28, 15,  6, 21, 10,
				         23, 19, 12,  4, 26,  8,
			        	 16,  7, 27, 20, 13,  2,
			        	 41, 52, 31, 37, 47, 55,
			        	 30, 40, 51, 45, 33, 48,
			        	 44, 49, 39, 56, 34, 53,
			        	 46, 42, 50, 36, 29, 32 };
  
  string modify(const int pc[], int size);
  /* left circular shift */
  string shift(string s, int n);
  /* generate 16 keys for each round */
  void generate_keys();

  /* plaintext operations */

  /* Initial Permutation table */
  const int IP[64] = { 58, 50, 42, 34, 26, 18, 10, 2,  
				       60, 52, 44, 36, 28, 20, 12, 4,
				       62, 54, 46, 38, 30, 22, 14, 6,
				       64, 56, 48, 40, 32, 24, 16, 8,
				       57, 49, 41, 33, 25, 17,  9, 1,
				       59, 51, 43, 35, 27, 19, 11, 3,
				       61, 53, 45, 37, 29, 21, 13, 5,
				       63, 55, 47, 39, 31, 23, 15, 7 };
  
  /* Permutation table */
  const int P[32] = { 16,  7, 20, 21,
				      29, 12, 28, 17,
				       1, 15, 23, 26,
				       5, 18, 31, 10,
				       2,  8, 24, 14,
				      32, 27,  3,  9,
				      19, 13, 30,  6,
				      22, 11,  4, 25 };

  /* Final Permutation table */
  const int FP[64] = { 40, 8, 48, 16, 56, 24, 64, 32,
				       39, 7, 47, 15, 55, 23, 63, 31,
				       38, 6, 46, 14, 54, 22, 62, 30,
				       37, 5, 45, 13, 53, 21, 61, 29,
				       36, 4, 44, 12, 52, 20, 60, 28,
				       35, 3, 43, 11, 51, 19, 59, 27,
				       34, 2, 42, 10, 50, 18, 58, 26,
				       33, 1, 41,  9, 49, 17, 57, 25 };

  /* expansion table */
  const int E[48] = { 32,  1,  2,  3,  4,  5, 
				       4,  5,  6,  7,  8,  9,
				       8,  9, 10, 11, 12, 13,
				      12, 13, 14, 15, 16, 17,
				      16, 17, 18, 19, 20, 21,
				      20, 21, 22, 23, 24, 25,
				      24, 25, 26, 27, 28, 29,
				      28, 29, 30, 31, 32,  1 };

  /* substitution boxes */
  const int S[8][4][16] = {                      
		{
			{ 14,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7 },
			{ 0,15,7,4,14,2,13,1,10,6,12,11,9,5,3,8 },
			{ 4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0 },
			{ 15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13 }
		},
		{
			{ 15,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10 },
			{ 3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5 },
			{ 0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15 },
			{ 13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9 }
		},
		{
			{ 10,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8 },
			{ 13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1 },
			{ 13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7 },
			{ 1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12 }
		},
		{
			{ 7,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15 },
			{ 13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9 },
			{ 10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4 },
			{ 3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14 }
		},
		{
			{ 2,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9 },
			{ 14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6 },
			{ 4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14 },
			{ 11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3 }
		},
		{
			{ 12,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11 },
			{ 10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8 },
			{ 9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6 },
			{ 4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13 }
		},
		{
			{ 4,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1 },
			{ 13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6 },
			{ 1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2 },
			{ 6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12 }
		},
		{
			{ 13,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7 },
			{ 1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2 },
			{ 7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8 },
			{ 2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11 }
		}
	};

    /* functions */
    /* apply permutation */
    string modify(string s, const int p[], int size);  

};

string DES::modify(const int pc[], int size)
{
    string perm_key = "";
    for (int i = 0; i < size; i++)
    {
        perm_key += _key[pc[i] - 1];
    }
    return perm_key;
} 

string DES::shift(string s, int n)
{
    string res = "";
    for (int i = n; i < s.size(); i++)
    {
        res += s[i];
    }
    for (int i = 0; i < n; i++)
    {
        res += s[i];
    }
    return res;
}


void DES::generate_keys()
{
  /* compress the key */
  string perm_key = modify(pc_1, sizeof(pc_1)/sizeof(pc_1[0]));
  /* divide the key into halves */
  string lpk = perm_key.substr(0, 28); 
  string rpk = perm_key.substr(28, 28);
  /* generate 16 keys for each round (make shifts) */
  for (int i = 0; i < n_rounds; i++)
  {
    lpk = shift(lpk, key_shift[i]);
    rpk = shift(rpk, key_shift[i]);
  
    /* combine the keys */
    string comb_key = lpk + rpk;
    /* compress via pc_2 table and save it*/
    _keys[i] = modify(pc_2, sizeof(pc_2)/sizeof(pc_2[0]));
  } 
}

string DES::modify(string s, const int p[], int size)
{
    string res = "";
    for (int i = 0; i < size; i++)
    {
        res += s[p[i] - 1];
    }
    return res;
}

string DES::_encrypt(string pt)
{
    /* initial permutation */
    string perm_pt = modify(pt, IP, sizeof(IP)/sizeof(IP[0]));
    /* divide the permuted plaintext into halves */
    string lpt = perm_pt.substr(0, 28); 
    string rpt = perm_pt.substr(28, 28);

    /* each half encrypted 16 (n_rounds) times */
    for (int i = 0; i < n_rounds; i++)
    {
        /* right expansion */
        rpt = modify(rpt, E, sizeof(E)/sizeof(E[0]));
        /* result is XOR-ed with a key */
        rpt = XOR(_keys[i], rpt);
        /* result divided into 8 equal parts, passed through 8 substitution boxes */
        string res = "";
        for (int j = 0; j < 8; j++)
        {
            /* indices to look for in the substitution box */
            string srow = rpt.substr(j*6, 1)   + rpt.substr(j*6+5, 1);
            int row = bin_to_dec(srow);

            string scol = rpt.substr(j*6+1, 1) + rpt.substr(j*6+2, 1) + rpt.substr(j*6+3, 1) + rpt.substr(j*6+4, 1);
            int col = bin_to_dec(scol);

            int val = S[j][row][col];
            res += dec_to_bin(val);
        }
        /* another permutation */
        res = modify(res, P, sizeof(P)/sizeof(P[0]));

        /* result XOR-ed with the left part (it is the new lpt)*/
        res = XOR(res, lpt);
        
        /* swap left and right parts */
        if (i < n_rounds - 1)
        {
            string temp = rpt;
            rpt = res;
            lpt = temp;
        }
    }

    /* combine the halves */
    string combined = lpt + rpt;
    /* final permutation */
    string cypher = modify(combined, FP, sizeof(FP)/sizeof(FP[0]));
    return cypher;
}

string DES::encrypt(string plaintext)
{
    /* modify the plaintext */
    string pt = hex_to_bin(plaintext);
    string c = _encrypt(pt);
    return bin_to_hex(c);
}


/* make it more useful */
int main()
{
    string pt;
    string key;
    cin >> key;
    DES des(key);
    cin >> pt;
    string c = des.encrypt(pt);
    cout << c << endl;

    return 0;
}
