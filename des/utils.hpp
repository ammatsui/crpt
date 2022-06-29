#ifndef UTILS_H   
#define UTILS_H

/* binary, hex, and decimal convertions */
/* binary = string of 0 and 1 */

#include <cmath>

std::string XOR(std::string x, std::string y);

std::string hex_to_bin(std::string x);
std::string bin_to_hex(std::string x);
std::string dec_to_bin(int d);
int bin_to_dec(std::string x);

std::string XOR(std::string x, std::string y)
{
    std::string res = ""; 
	int size = y.size();
	for (int i = 0; i < size; i++)
    { 
		if(x[i] != y[i])
        { 
			res += "1"; 
		}
		else
        { 
			res += "0"; 
		} 
	} 
	return res; 
}

std::string hex_to_bin(std::string x)
{
    std::string bin = "";
	for (int i = 0; i < x.size(); i++)
	{
		switch (x[i])
		{
		case '0': bin += "0000"; break;
		case '1': bin += "0001"; break;
		case '2': bin += "0010"; break;
		case '3': bin += "0011"; break;
		case '4': bin += "0100"; break;
		case '5': bin += "0101"; break;
		case '6': bin += "0110"; break;
		case '7': bin += "0111"; break;
		case '8': bin += "1000"; break;
		case '9': bin += "1001"; break;
		case 'A':
		case 'a': bin += "1010"; break;
		case 'B':
		case 'b': bin += "1011"; break;
		case 'C':
		case 'c': bin += "1100"; break;
		case 'D':
		case 'd': bin += "1101"; break;
		case 'E':
		case 'e': bin += "1110"; break;
		case 'F':
		case 'f': bin += "1111"; break;

		}
	}
	return bin;
}

std::string bin_to_hex(std::string x)
{
    std::string hex = "";
	for (int i = 0; i < x.size(); i += 4)
	{
		std::string k = "";
		for (int j = i; j < i + 4; j++)
			k += x[j];
		if (k == "0000")
			hex += '0';
		else if (k == "0001")
			hex += '1';
		else if (k == "0010")
			hex += '2';
		else if (k == "0011")
			hex += '3';
		else if (k == "0100")
			hex += '4';
		else if (k == "0101")
			hex += '5';
		else if (k == "0110")
			hex += '6';
		else if (k == "0111")
			hex += '7';
		else if (k == "1000")
			hex += '8';
		else if (k == "1001")
			hex += '9';
		else if (k == "1010")
			hex += 'A';
		else if (k == "1011")
			hex += 'B';
		else if (k == "1100")
			hex += 'C';
		else if (k == "1101")
			hex += 'D';
		else if (k == "1110")
			hex += 'E';
		else if (k == "1111")
			hex += 'F';
	}
	return hex;

}

std::string dec_to_bin(int d)
{
    std::string x;
    while(d != 0) 
    {
		x = (d % 2 == 0 ? "0" : "1") + x; 
		d = d/2;
	}

	while(x.length() < 4)
    {
		x = "0" + x;
	}
    return x;
}

int bin_to_dec(std::string x)
{
    int d = 0;
	int counter = 0;
	int size = x.length();
	for(int i = size-1; i >= 0; i--)
	{
    	if(x[i] == '1'){
        	d += pow(2, counter);
    	}
    counter++;
	}
	return d;
}


#endif