
local Byte         = string.byte;
local Char         = string.char;
local Sub          = string.sub;
local Concat       = table.concat;
local Insert       = table.insert;
local LDExp        = math.ldexp;
local GetFEnv      = getfenv or function() return _ENV end;
local Setmetatable = setmetatable;
local Select       = select;

local Unpack = unpack or table.unpack;
local ToNumber = tonumber;local function decompress(b)local c,d,e="","",{}local f=256;local g={}for h=0,f-1 do g[h]=Char(h)end;local i=1;local function k()local l=ToNumber(Sub(b, i,i),36)i=i+1;local m=ToNumber(Sub(b, i,i+l-1),36)i=i+l;return m end;c=Char(k())e[1]=c;while i<#b do local n=k()if g[n]then d=g[n]else d=c..Sub(c, 1,1)end;g[f]=c..Sub(d, 1,1)e[#e+1],c,f=d,d,f+1 end;return table.concat(e)end;local ByteString=decompress('24723Y27523W23U27523Y26H26N26B26J23W23O27925L26J26Y26526J27026W26F26L27E24E27926327127M25J26827226Z27K27M27O27Q27F27926426Z26827L27N27P27E23R27926X26927026D27127226N28423S27925P27C27M26N23W23T27925T25T26F26826I26J26U23W23X27926826J26X23W23P27H27J26026F29826526F26S27E29B27527I26Y26626927126F26Y26F26926828529M27J26326829S26426N26V23W28E29Y26Y25D27026F26H28Z23W23Q29C26Y25M26J26A26Y28T27S27526526L27026J26J26829P28Z26Y2622692A32A528V27528Q27126Y2B123W28O2A825I29S2A62AH26226N27026H27J23W23L2BF2BH2BJ26Y25K26F2AL27M2BL27927L2AY2BO27J2BR2BT27023W23M2AH2B125E2AU26H26Y26E2C42BW27J2C72C92CB23W23N2AH2C126Y27M26226V27227E2CK2AP2C02BS2CN2702CP2CR29X23Y25N26826N26K26A26J25J26L29V29A27925M26F2712D52D72D929V25127924C25627923Z27924U27927923W23Y24C27623Y2952DT2DU23Y2DV2DX23Y2DQ2DQ2752DS2E62EB2E42752952782E12DQ2952ED2E323Y23V2DR2DY2DV28O23W25K2DZ2E123Y25723Y2EG27924E25D2EY23Y28V27823S24M2EM2EB2EI2752662DP2DT2FD2EE2FF2792EZ2F12F32AG2EN2F72F92DV2E72FC2E22752FL27828E2FO2F82EN2952FS23Y2FG2E12FW23Y27G2FZ2F92782G32G52DV2FV2F227829B2EN2ES2F92EN2GG2F32C52GK2ET2FO2F02GH23Y2CK2GR2F92B323Y2G723K2FQ2GS23Y2AG2GO2782BM2GY2EN2A72H12GV27S2HB2G82GU2F324F2H42F929L2HE2F32DX2HH2C52H823Y24D2HM2EN2CT2HP27824A2HX23Y2H32HU24B2I32BM2HU2482I32AO2I023Y2492F92FP2EN2HL2GD2EV2IE2462IH2G02DW2FA2FT2EV2G72742GN24J2F32GF27525G2FE2752HW2752H72E32GN23Y29T26L28J2E02E125Q2JE2E12BV27526L26Z2702AS26829O29Q29S29U29W2HO29N29P29R29T2DB2ID2722AS27O26926Z2712JX2JS29V2JH27524Q2JG2EV23I24X2ID24U28V2J02DQ24U2582EL2DV2DV24O2452EL2AG2FB23U2HT2H62EB24U24O2EL2752GN2I22KQ2EL2FL2752782DV2KX27929B2KM2L22J02L52KP2L82F22752ER23Y2KJ2L32L02KO2L728V24U2542LK2AG2L92EC2J12J32JI2I22762HL27526G2CW27M2AJ27127Q29026N2JP2712D12M92C22CZ27E2EZ2D326Z26B23W2M723Y2B126L26N2B62CM2CO2CQ28D27925O26A28M26D26A2DF26Y2BE2752702A42C82682CA26E2KB25Q26P25Q2JJ23Y2JL2JN2AU2JQ2JY2JT28U27926026J26L26Y28H2582942962982KB2DU2K12K329U2K62K82JZ29W2J92JB2JD2KF24X23W2ID25O28Z26I2AZ2642AU2912702652CN27223W2M523Y25F26J2MX2JM26J25F2K527V2P526W26J27D2JP23W2I52MU2OR27M2662AB28H29S2A52J927X27Z2NA2J926026N26A26Z27E2KY27V26Y27D2AM2AM2D626J2L22792552IM2M32FV2DT2FS2E52FB2E92752FS2782EA2FS2EN2ID2EL2DQ25A2DT28O28V2QD23Y2HD2QJ2EB27G2E52DV2HO2E52952HO2FE2EJ2H12DT2KV2IR2EB2QV2QG2QX2RA2R02DM2FH2R42RD2R62QM2C52QO2DT2CK2PE2EB2DT2QW2DQ2BM2R22R72752E52782JE2EA2782782HL2S32HQ2LP2LA2F32HW2II2FE2GF2PE2E82IT2EV25E2EB2EN2DQ2QT2OY2FU2EV2R62EH2EU2SH2SQ2ST2RD2EV2J92F92SV2DT2FR2SR2Q82FB2T22RS2ST2SH2CD27529E29827228H27K29I26J2EZ23Y25P2T92E928V2FS2E82IY2DQ2J023Y2J22SY2EH2TB2ID29N2P52K626J25E2692MW2OD2T024S2TO2S92IS2SV2TU2EV2T82QB2IM23Z2EZ2DC2A82OC2NU2E523Y2TE26X2TG2702JQ2AX2OP2A423W2SH2932SH26V2QV2BM2SH2UF2LK2SW2UD2L72DV2S72FB2EA2SM2F32E22T02DY2Q82UC2QG2J02D129N2A02A22V02BA23Y2AA2AC28Z2J924U2HW2V82UJ2L72RU2LR2M12UK2UK2VQ2I92NO2JM2JO2NS2K929W2O826J2K42OB2JR2U92DR25C2T92KM2VM2W82FS2TV2UG2WA2UH2J72CJ2862A42MW2B62662BH27C2MI2JE2972992442792MZ2702MC2ME26I2MG26Y27127X2B62MG27Q2MI2MT2MK2CX2XI2OR2XL2MI27G2752XG2MM2772792MP2MR2MT2MV2MX2BQ2MA2CY2N12NB23Y2N42N62N82B62R124Q2IS2QF2IS2WX2M12EA2J02782AO2FB2EK2EQ2EO2LK2LV2EP2KZ2YV2FQ2W52T62UK2H02OK2BW2AR2AT2AV2692UY2B02V02HD28G28I28K28M27E2H02Y92B62YF29N2VY2AD29W2Y023Y2VS2A12B72ZI2DD26F2AS2NZ2NU2HD2ND26V2NF2NH2WD24U24K2WS2Z62YX2LR2YS2EL2952S72SA2FJ2LK2VH2792T02GK2EL310R2GT2EA2VW2H02GM23Y2VW31122VO2YY2JI2VN2VO2E62VQ2H02B531012A52J72UP29V2HO2L1310G23Y2I22UG2EQ2I72DQ2VI2DQ2LD2L42EB311U2KZ2KM2KK2M123S2I72DV28E2QH2KY31252752532VA2752IY2L72792TX311923Z311B28P2YA2B82J72XO2AM26827Q2ZX311K2W4311M2M12EQ25B2EB2TQ2FJ2KY31302LQ2T4311P2KU2IS311X2R923Y312A312E312C312B2TW2Q92DP2TB2XT2YC2XW2MF2MH2S231302SY2TT2W8312G2UI23Z2VW2B927926Y2N126926G2AF279312O2XQ27E313K2C2313M2XK2MH23W2OF2DH27E2J927M27028H23W24X27926926K26C2NY26Y24U2922CR2NZ26J26I24U2MG24U28Z2XP312Q26J24U28H315024U2Q22D7315726G315226831542XR24I24U310526J26F2P926I25024U2IB2RZ25L2QN2DT2DX2G22DT2L62LK24O2UG28O2UG311X31642EY2KY2LF2752EX315Y2RH2GC2DT2422EU316F2M02952EF2KY311Q23Y31622EL2H3311T3168279316S316U2QA2DQ316K311W316N2EB24C315U2952J92S031142RH310U2792QP2S42F923Y24L2EU2T4313G2TY2T32TB31082NE2CH2NI2DR313Q2RS313S2TV313U2UK313X2VW31402CR3142313Y2752MX27V2UW318423Y2682MQ26K27M2O62762YM2752D72NG2CB314U26U314W2CN314Z26N24U318B26B318D270315J315L315N314Y315Q2YF3109310B2CB2MT24C264315V2E1316D2T32LK2RX2J02Q52DV3130316M2793130311X312823K2DS27827G3126279319R2QA2TL27525R2F32SH24C3176317F2QQ2W8311V2GT27931A2316Z31A42T82UI2V12DU2ZX2XU2N02CR313P311L317V2DT317X2X02DU2MO318T2MS2X326V2X52YB2ML2YE2KY29N2Y625J2CN26B2MI314F2Q32Y42M82ON2MJ2YC2Y3314I2JN314L318H23Y27X26W2PT26F314Z310931AW24U31AH270315A2N124U2K226927O29126I2UR24C314N317J2VC310Q2Z22JH2VD2RA2EF2YZ316L31A32IZ2SZ2VO31A231162952RN317127931CM311X316W2M03167311X2IK2DW31752F42RH2FM2DT317H2R62E831AO2JI2VQ2ID2U327V2DJ2682D3314G26I2E62JH2TN2T225A2IO2WW2M2317L2DP31D727931D92D82DA31DC2D42Q331DF2EU28P2UC31DK2YP31DN312H23Y');

local BitXOR = bit and bit.bxor or function(a,b)
    local p,c=1,0
    while a>0 and b>0 do
        local ra,rb=a%2,b%2
        if ra~=rb then c=c+p end
        a,b,p=(a-ra)/2,(b-rb)/2,p*2
    end
    if a<b then a=b end
    while a>0 do
        local ra=a%2
        if ra>0 then c=c+p end
        a,p=(a-ra)/2,p*2
    end
    return c
end

local function gBit(Bit, Start, End)
	if End then
		local Res = (Bit / 2 ^ (Start - 1)) % 2 ^ ((End - 1) - (Start - 1) + 1);
		return Res - Res % 1;
	else
		local Plc = 2 ^ (Start - 1);
        return (Bit % (Plc + Plc) >= Plc) and 1 or 0;
	end;
end;

local Pos = 1;

local function gBits32()
    local W, X, Y, Z = Byte(ByteString, Pos, Pos + 3);

	W = BitXOR(W, 142)
	X = BitXOR(X, 142)
	Y = BitXOR(Y, 142)
	Z = BitXOR(Z, 142)

    Pos	= Pos + 4;
    return (Z*16777216) + (Y*65536) + (X*256) + W;
end;

local function gBits8()
    local F = BitXOR(Byte(ByteString, Pos, Pos), 142);
    Pos = Pos + 1;
    return F;
end;

local function gBits16()
    local W, X = Byte(ByteString, Pos, Pos + 2);

	W = BitXOR(W, 142)
	X = BitXOR(X, 142)

    Pos	= Pos + 2;
    return (X*256) + W;
end;

local function gFloat()
	local Left = gBits32();
	local Right = gBits32();
	local IsNormal = 1;
	local Mantissa = (gBit(Right, 1, 20) * (2 ^ 32))
					+ Left;
	local Exponent = gBit(Right, 21, 31);
	local Sign = ((-1) ^ gBit(Right, 32));
	if (Exponent == 0) then
		if (Mantissa == 0) then
			return Sign * 0; -- +-0
		else
			Exponent = 1;
			IsNormal = 0;
		end;
	elseif (Exponent == 2047) then
        return (Mantissa == 0) and (Sign * (1 / 0)) or (Sign * (0 / 0));
	end;
	return LDExp(Sign, Exponent - 1023) * (IsNormal + (Mantissa / (2 ^ 52)));
end;

local gSizet = gBits32;
local function gString(Len)
    local Str;
    if (not Len) then
        Len = gSizet();
        if (Len == 0) then
            return '';
        end;
    end;

    Str	= Sub(ByteString, Pos, Pos + Len - 1);
    Pos = Pos + Len;

	local FStr = {}
	for Idx = 1, #Str do
		FStr[Idx] = Char(BitXOR(Byte(Sub(Str, Idx, Idx)), 142))
	end

    return Concat(FStr);
end;

local gInt = gBits32;
local function _R(...) return {...}, Select('#', ...) end

local function Deserialize()
    local Instrs = {};
    local Functions = {};
	local Lines = {};
    local Chunk = 
	{
		Instrs,
		Functions,
		nil,
		Lines
	};
	local ConstCount = gBits32()
    local Consts = {}

	for Idx=1, ConstCount do 
		local Type =gBits8();
		local Cons;
	
		if(Type==0) then Cons = (gBits8() ~= 0);
		elseif(Type==3) then Cons = gFloat();
		elseif(Type==2) then Cons = gString();
		end;
		
		Consts[Idx] = Cons;
	end;
for Idx=1,gBits32() do 
									local Descriptor = gBits8();
									if (gBit(Descriptor, 1, 1) == 0) then
										local Type = gBit(Descriptor, 2, 3);
										local Mask = gBit(Descriptor, 4, 6);
										
										local Inst=
										{
											gBits16(),
											gBits16(),
											nil,
											nil
										};
	
										if (Type == 0) then 
											Inst[3] = gBits16(); 
											Inst[4] = gBits16();
										elseif(Type==1) then 
											Inst[3] = gBits32();
										elseif(Type==2) then 
											Inst[3] = gBits32() - (2 ^ 16)
										elseif(Type==3) then 
											Inst[3] = gBits32() - (2 ^ 16)
											Inst[4] = gBits16();
										end;
	
										if (gBit(Mask, 1, 1) == 1) then Inst[2] = Consts[Inst[2]] end
										if (gBit(Mask, 2, 2) == 1) then Inst[3] = Consts[Inst[3]] end
										if (gBit(Mask, 3, 3) == 1) then Inst[4] = Consts[Inst[4]] end
										
										Instrs[Idx] = Inst;
									end
								end;for Idx=1,gBits32() do Functions[Idx-1]=Deserialize();end;Chunk[3] = gBits8();return Chunk;end;
local function Wrap(Chunk, Upvalues, Env)
	local Instr  = Chunk[1];
	local Proto  = Chunk[2];
	local Params = Chunk[3];

	return function(...)
		local Instr  = Instr; 
		local Proto  = Proto; 
		local Params = Params;

		local _R = _R
		local InstrPoint = 1;
		local Top = -1;

		local Vararg = {};
		local Args	= {...};

		local PCount = Select('#', ...) - 1;

		local Lupvals	= {};
		local Stk		= {};

		for Idx = 0, PCount do
			if (Idx >= Params) then
				Vararg[Idx - Params] = Args[Idx + 1];
			else
				Stk[Idx] = Args[Idx + 1];
			end;
		end;

		local Varargsz = PCount - Params + 1

		local Inst;
		local Enum;	

		while true do
			Inst		= Instr[InstrPoint];
			Enum		= Inst[1];if Enum <= 40 then if Enum <= 19 then if Enum <= 9 then if Enum <= 4 then if Enum <= 1 then if Enum == 0 then Stk[Inst[2]] = Inst[3];else if(Stk[Inst[2]] ~= Inst[4]) then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;end; elseif Enum <= 2 then InstrPoint=Inst[3]; elseif Enum == 3 then local A;local K;local B;Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];B=Inst[3];K=Stk[B] for Idx=B+1,Inst[4] do K=K..Stk[Idx];end;Stk[Inst[2]]=K;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;else Stk[Inst[2]]=(Inst[3]~=0);end; elseif Enum <= 6 then if Enum > 5 then 
local A = Inst[2]
Stk[A] = Stk[A]()
else local A;Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 7 then Stk[Inst[2]]=Stk[Inst[3]][Inst[4]]; elseif Enum > 8 then local B=Inst[3];local K=Stk[B] for Idx=B+1,Inst[4] do K=K..Stk[Idx];end;Stk[Inst[2]]=K;else for Idx=Inst[2],Inst[3] do Stk[Idx]=nil;end;end; elseif Enum <= 14 then if Enum <= 11 then if Enum > 10 then Stk[Inst[2]]=(Inst[3]~=0);InstrPoint=InstrPoint+1;else if Stk[Inst[2]] then InstrPoint=InstrPoint + 1; else InstrPoint = Inst[3]; end;end; elseif Enum <= 12 then InstrPoint=Inst[3]; elseif Enum > 13 then Stk[Inst[2]]=Upvalues[Inst[3]];else Stk[Inst[2]]=Stk[Inst[3]]-Stk[Inst[4]];end; elseif Enum <= 16 then if Enum > 15 then Stk[Inst[2]]=(Inst[3]~=0);InstrPoint=InstrPoint+1;else local B;local A;A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]; 
do return Unpack(Stk, A, Top) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 17 then 
do return Stk[Inst[2]] end
 elseif Enum == 18 then Stk[Inst[2]]=Stk[Inst[3]];else local B;local A;A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
do return Stk[Inst[2]] end
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 29 then if Enum <= 24 then if Enum <= 21 then if Enum > 20 then if not Stk[Inst[2]] then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;else 
local A = Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
end; elseif Enum <= 22 then if(Stk[Inst[2]] ~= Inst[4]) then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end; elseif Enum == 23 then 
local A = Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
else Stk[Inst[2]][Inst[3]] = Inst[4];end; elseif Enum <= 26 then if Enum > 25 then Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];else Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];end; elseif Enum <= 27 then if(Stk[Inst[2]] == Inst[4])then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end; elseif Enum > 28 then Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];else 
local A = Inst[2];
local T = Stk[A];
for Idx = A + 1, Inst[3] do 
	Insert(T, Stk[Idx])
end;end; elseif Enum <= 34 then if Enum <= 31 then if Enum == 30 then 
local A = Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;else Stk[Inst[2]]=Env[Inst[3]];end; elseif Enum <= 32 then if (Inst[2] <= Stk[Inst[4]]) then InstrPoint=Inst[3]; else InstrPoint=InstrPoint+1; end; elseif Enum > 33 then local B;local A;Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]; 
do return Unpack(Stk, A, Top) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;else Stk[Inst[2]]=Stk[Inst[3]]*Stk[Inst[4]];end; elseif Enum <= 37 then if Enum <= 35 then local NewProto=Proto[Inst[3]];local NewUvals;local Indexes={};NewUvals=Setmetatable({},{__index=function(_,Key)local Val=Indexes[Key];return Val[1][Val[2]];end,__newindex=function(_,Key,Value)local Val=Indexes[Key] Val[1][Val[2]]=Value;end;});for Idx=1,Inst[4] do InstrPoint=InstrPoint+1;local Mvm=Instr[InstrPoint];if Mvm[1]==80 then Indexes[Idx-1]={Stk,Mvm[3]};else Indexes[Idx-1]={Upvalues,Mvm[3]};end;Lupvals[#Lupvals+1]=Indexes;end;Stk[Inst[2]]=Wrap(NewProto,NewUvals,Env); elseif Enum == 36 then local A;Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A]()
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
do return Stk[Inst[2]] end
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;else Stk[Inst[2]][Inst[3]] = Inst[4];end; elseif Enum <= 38 then local A=Inst[2];local B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]]; elseif Enum > 39 then local NewProto=Proto[Inst[3]];local NewUvals;local Indexes={};NewUvals=Setmetatable({},{__index=function(_,Key)local Val=Indexes[Key];return Val[1][Val[2]];end,__newindex=function(_,Key,Value)local Val=Indexes[Key] Val[1][Val[2]]=Value;end;});for Idx=1,Inst[4] do InstrPoint=InstrPoint+1;local Mvm=Instr[InstrPoint];if Mvm[1]==80 then Indexes[Idx-1]={Stk,Mvm[3]};else Indexes[Idx-1]={Upvalues,Mvm[3]};end;Lupvals[#Lupvals+1]=Indexes;end;Stk[Inst[2]]=Wrap(NewProto,NewUvals,Env);else 
local A = Inst[2]
Stk[A] = Stk[A]()
end; elseif Enum <= 61 then if Enum <= 50 then if Enum <= 45 then if Enum <= 42 then if Enum > 41 then local B;local A;A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]]*Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]; 
do return Unpack(Stk, A, Top) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;else do return end;end; elseif Enum <= 43 then 
local A = Inst[2]
Stk[A](Stk[A + 1])
 elseif Enum == 44 then 
local A = Inst[2]; 
do return Unpack(Stk, A, Top) end;else 
do return Stk[Inst[2]] end
end; elseif Enum <= 47 then if Enum == 46 then Stk[Inst[2]]=Stk[Inst[3]]-Stk[Inst[4]];else local A=Inst[2];local B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];end; elseif Enum <= 48 then local B=Inst[3];local K=Stk[B] for Idx=B+1,Inst[4] do K=K..Stk[Idx];end;Stk[Inst[2]]=K; elseif Enum == 49 then if Stk[Inst[2]] then InstrPoint=InstrPoint + 1; else InstrPoint = Inst[3]; end;else Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];end; elseif Enum <= 55 then if Enum <= 52 then if Enum == 51 then 
local A = Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
else local B;local A;Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];end; elseif Enum <= 53 then Stk[Inst[2]]={}; elseif Enum == 54 then Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];else local B;local A;Stk[Inst[2]]={};InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]={};InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Inst[4];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Inst[4];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];end; elseif Enum <= 58 then if Enum <= 56 then if not Stk[Inst[2]] then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end; elseif Enum == 57 then for Idx=Inst[2],Inst[3] do Stk[Idx]=nil;end;else 
local A = Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
end; elseif Enum <= 59 then Stk[Inst[2]] = Stk[Inst[3]] % Inst[4]; elseif Enum == 60 then 
local A = Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;else 
local A = Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
end; elseif Enum <= 71 then if Enum <= 66 then if Enum <= 63 then if Enum > 62 then local B;local A;Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];if not Stk[Inst[2]] then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;else if(Stk[Inst[2]] == Inst[4])then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;end; elseif Enum <= 64 then 
local A = Inst[2]
Stk[A](Stk[A + 1])
 elseif Enum == 65 then Stk[Inst[2]]=(Inst[3]~=0);else Stk[Inst[2]]=Wrap(Proto[Inst[3]],nil,Env);end; elseif Enum <= 68 then if Enum == 67 then Stk[Inst[2]]=Upvalues[Inst[3]];else 
local A = Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
end; elseif Enum <= 69 then 
local A = Inst[2];
local T = Stk[A];
for Idx = A + 1, Inst[3] do 
	Insert(T, Stk[Idx])
end; elseif Enum == 70 then Stk[Inst[2]]=Wrap(Proto[Inst[3]],nil,Env);else Stk[Inst[2]]=Env[Inst[3]];end; elseif Enum <= 76 then if Enum <= 73 then if Enum == 72 then 
local A = Inst[2]; 
do return Unpack(Stk, A, Top) end;else Stk[Inst[2]]={};end; elseif Enum <= 74 then do return end; elseif Enum > 75 then local A;
A= Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
do return Stk[Inst[2]] end
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;else Stk[Inst[2]]=Stk[Inst[3]]*Stk[Inst[4]];end; elseif Enum <= 79 then if Enum <= 77 then if (Inst[2] <= Stk[Inst[4]]) then InstrPoint=Inst[3]; else InstrPoint=InstrPoint+1; end; elseif Enum > 78 then Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];else Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]]-Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
do return Stk[Inst[2]] end
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 80 then Stk[Inst[2]]=Stk[Inst[3]]; elseif Enum == 81 then Stk[Inst[2]] = Inst[3];else local A;Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];if(Stk[Inst[2]] ~= Inst[4]) then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;end;
			InstrPoint	= InstrPoint + 1;
		end;
    end;
end;	
return Wrap(Deserialize(), {}, GetFEnv())();
