
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
local ToNumber = tonumber;local function decompress(b)local c,d,e="","",{}local f=256;local g={}for h=0,f-1 do g[h]=Char(h)end;local i=1;local function k()local l=ToNumber(Sub(b, i,i),36)i=i+1;local m=ToNumber(Sub(b, i,i+l-1),36)i=i+l;return m end;c=Char(k())e[1]=c;while i<#b do local n=k()if g[n]then d=g[n]else d=c..Sub(c, 1,1)end;g[f]=c..Sub(d, 1,1)e[#e+1],c,f=d,d,f+1 end;return table.concat(e)end;local ByteString=decompress('26P26G27526I26K27526G23Z23T23P23X26I26A27924V23X24C25723X24A24E23L23V27E26W27925924B27M24H23Q24824D27K27M27O27Q27F27925624D23Q27L27N27P27E26927924F23R24A23N24B24823T28426M27924R27C27M23T26I26N27925325323L23Q23W23X24026I26J27923Q23X24F26I26B27H27J25A23L29825723L24227E29B27527I24C25423R24B23L24C23L23R23Q28529M27J25923Q29S25623T24126I28E29Y24C24N24A23L23Z28Z26I26829C24C24S23X23O24C28T27S27525723V24A23X23X23Q29P28Z24C25823R2A32A528V27528Q24B24C2B126I28O2A824G29S2A62AH25823T24A23Z27J26I26F2BF2BH2BJ24C24U23L2AL27M2BL27927L2AY2BO27J2BR2BT24A26I26C2AH2B124K2AU23Z24C23K2C42BW27J2C72C92CB26I26D2AH2C124C27M25824124827E2CK2AP2C02BS2CN24A2CP2CR29X26G24T23Q23T23U23O23X24H23V29V29A27924S23L24B2D52D72D929V25V27926Y2AO26G26H27925K27927926I26G26Y27626G2952DT2DU26G2DV2DX2DP2E62752DS2E62DQ2E42752952782E12DQ2952EC2E326G26L2DR2DY2DV28O26I24L2DZ2E126G26526G2EF27926W24N2EX26G28V27826M25Q2EL2EA2ET26G25Y2792DQ2792FC2ED2DT2FG2F22752F02F22AG2EM2F62F82DV2DQ2FE2FJ2DV2FL2F127828E2FP2F72EM2952FT2752FJ2DT2FM27827G2G12F82782G52FB2E22FX2F229B2EM2ER2F82EM2GI2782C52GL2ES2FP2EZ2FY26G2CK2GS2F82B326G2G926G26E2FR2GT26G2AG2GP26G2BM2GZ2EM2A72H22GW27S2HD26G27G2HA26X2H62F829L2HG2F22DX2HJ2C52HA26Z2HO2EM2CT2HR2782702HY2H42GV2F22712I42BM2HA2722I42DO2H32732F82FQ2EM2HN2GF2G72I627826O2IH2G22DW2F92GG2EU2H32742GO2642F22FW27524H2FD2792HX2752H92E32GO26G29T23V28J2FE2E124O2JF2E12BV27523V24D24A2AS23Q29O29Q29S29U29W2HQ29N29P29R29T2DB2DO2482AS27O23R24D24B2JY2JT29V2JI27525O2JH2EU1O25Z2HN2E825F2EK2752DQ25K2I32DV2KR26G25Q26R2EK2EM2EH2EX2J12KX2KZ27928E2KO2742L02752602EK2KS2FM2752782DV26K2J12C52L52KM2F826G2L92KS2DV2LC26G2EQ26G25K2KL2J12KO2KQ2EK28V25K2CK2KS2AG2LQ2EB2J22J42EU26I2I32762KJ26G23Y2CW27M2AJ24B27Q29023T2JQ24B2D12MF2C22CZ27E2EY2D324D23P26I2MD2B123V23T2B62CM2CO2CQ28D27924Q23O28M23N23O2DF24C2BE27524A2A42C823Q2CA23K2KC24O24724O2JK26G2JM2JO2AU2JR2JZ2JU28U27925A23X23V24C28H2622942962982KC2DU2K22K429U2K72K92K029W2JA2JC2JE2KG25Z26I2DO24Q28Z23W2AZ2562AU29124A2572CN2482MA27924L23X2N22JN23X24L2K627V2P924E23X27D2JQ26I2H52752OV29027M2542AB28H29S2A52JA27X27Z2NF2JA25A23T23O24D27E2HV26G27V24C27D2AM2AM2D623X2F727925L2EU2DT2KC2DO2F92E52KY2E82KN2E62782E92G52EM2QH2FS2LM2DT28O28V2QJ26G2HF2QP2E627G2E52DV2HQ2E52952HQ2J42EI2H22DT2AG2DQ2QY2R02QM2DQ2R32DY2R62FH2R92QM2RB2QH2LI2QU2792CK2PI2QM2DT2R12DQ2BM2R72RC2752E52782E02LT2LD2F22HN2E92782782DX2SC2F22HX2II2J42FW2MD2JI2IM2792I82DQ2L12QY2MB2DT2J12E12RB2EG2ET2OB2SW2QF2M82E12JA2IT2M92E72DT2S72SZ2G42EU2OB2T72CD27529E29824828H27K29I23X2EY2HB2T92KK2F92FE2IZ2M62792J32RX2EG2JF2OP2AH2P92K723X24K23R2N12OI2T625R2TS2S82TU2T32T22UE2SZ2TE2EY2DC2A82OH2NZ2E526G2TJ24F2TL24A2JR2AX2OU2A426I2JF2932JF2412R024O2T92JI2M62SW2T22E92LO2UF2GE2ET2E92L12TQ2DV2T62DY2QE2U02QF2J12D129N2A02A22V02BA26G2AA2AC28Z2JA25K28V2JF2V92LA2UF2G52SW2TX2TE2QE26H2VR2IA2NT2JN2JP2NX2KA29W2OD23X2K52OG2JS2UB2DR25I2V82UF2VR2T32WB2E62UI2VP2FD2J82CJ2862A42N12B62542BH27C2MO2S729729926Q2792N424A2MI2MK23W2MM24C24B27X2B62MM27Q2MO2MD2MQ2CX2XL2OW2XO2MO2HL26G2XJ2MS2772792MV2MX2MZ2X72N22BQ2MG2CY2N62NG26G2N92NB2ND2B62R62DX2GF2W82X02WY2EK2VD2792782AO2KY2EJ2EP2EN2W72M02EO2H82S32SR2LK2YS2WD2WD2H12U32AP2AR2AT2AV23R2UY2B02V02HF28G28I28K28M27E2H12N02YD2YI29N2VZ2AD29W2Y32VT2A12B72ZM2DD23L2AS2O42NZ2HF2NI2412NK2NM2WG25K24P2WV2SK2EK2R42LK2YV2DV2952SG2S92YW2W72VI2792T62GL2YU2752EM2GU2E92VX2H12GN2LR2QV2UJ2Z12JJ2VO2X22KN2VR2H12B531042A52J82UP29V2HQ25K2742W52L82YS2EP2622E62RE2YW2J1311W2LU2EK2M626M311U2DV311W2LD311Y2QN2J12L427525N2W72792IZ2KS2TY2T42VQ2DU311G2YD2B82J82XR2AM23Q27Q2Y3311O310J2L92VA2LR311U2DQ2W4311X279313231202QT2EP2LS311W2DQ2LG2793126312D312H275312G2Z92TZ2UJ2DP2U22XW2YF2XZ2ML2MN2S73120311Q26G2TW313J312J2FD2VX2B927924C2N623R23Y2AF2TY23Q2XS312S27E313O2C2313Q2XN2MN26I2OK2DH27E2JA27M24A28H26I25Z27923R23U23M2O324C25K2922CR2O423X23W25K2MM25K28Z314B27Q25K28H315525K2Q82D7315B23Y3157314A312R2XU25G25K310823X23L2PD23W25U25K2I82S426S31212DT26V2T02DT2LN2W725Q25E2EK27G2M6313C27531692752EW2TD27931612VG2792IC2EE2E62LQ2952EE2J12I32DQ2KU2EK2CK313B2L2275316W316Y2TR2DQ316O3129279316S2DW315Y2952JA26Y2C52F531602FR2DT24J2F22782GO316P315Z26G313K2TF2V12DU310B2NJ2CH23K313T2KL313V313X2SW317P2WD31412VX31442CR314631422752N227V2UW318926G23Q2MW23U27M2TF2E325O2792D72NL2CB314Z24031512CN315423T25K318G23P318I24A315N315P315R3153315U2YI310C310E2CB2MD26Y2AG2QT2SX31192W72S22J12QB2DV3132316Q31343175275312B26E24S2F2316D313B2J1316D26G24M2FK2DT25X2F22JF26Y24R31162E12LS2T52IH2TA2F8317331182OB317Q2U22Y32XX2N52CR317X310J31802DT31822TE2X42MU318Y2MY2X62412X82YE2MR2YH2Q229N2Y924H2CN23P2MO314K2Q92Y72752MF2902MP2YF2Y6314N2JO314Q318M2J223Q24E2PY23L3154310C31B025K31AL24A315E2N625K2K323R27O29123W319B27G319E2WW310U2Z52TB2VE2S22EY2E9317M2T62LE2EU2L1319F310Y2BA295317031272RU316N2F127528O319W2EZ2E626Y31792F32DM317D2Z6279317M319E31AS2JJ2VR2DO2U527V2DJ23Q2D3314L23W2DP2TB2BM2KC2602IP2W92M7311D2DP31DC2P32PA2D82DA31DH2D42Q931DK2FA27531DN2E131DP2F92FW31DA2T3');

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

	W = BitXOR(W, 232)
	X = BitXOR(X, 232)
	Y = BitXOR(Y, 232)
	Z = BitXOR(Z, 232)

    Pos	= Pos + 4;
    return (Z*16777216) + (Y*65536) + (X*256) + W;
end;

local function gBits8()
    local F = BitXOR(Byte(ByteString, Pos, Pos), 232);
    Pos = Pos + 1;
    return F;
end;

local function gBits16()
    local W, X = Byte(ByteString, Pos, Pos + 2);

	W = BitXOR(W, 232)
	X = BitXOR(X, 232)

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
		FStr[Idx] = Char(BitXOR(Byte(Sub(Str, Idx, Idx)), 232))
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
		elseif(Type==1) then Cons = gFloat();
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
			Enum		= Inst[1];if Enum <= 40 then if Enum <= 19 then if Enum <= 9 then if Enum <= 4 then if Enum <= 1 then if Enum == 0 then Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];else local A;Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 2 then InstrPoint=Inst[3]; elseif Enum == 3 then 
local A = Inst[2]
Stk[A](Stk[A + 1])
else 
local A = Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
end; elseif Enum <= 6 then if Enum > 5 then if not Stk[Inst[2]] then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;else 
do return Stk[Inst[2]] end
end; elseif Enum <= 7 then local B;local A;A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
do return Stk[Inst[2]] end
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end; elseif Enum == 8 then local A;Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];if(Stk[Inst[2]] ~= Inst[4]) then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;else Stk[Inst[2]]=Env[Inst[3]];end; elseif Enum <= 14 then if Enum <= 11 then if Enum > 10 then Stk[Inst[2]]={};else local B;local A;Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];if Stk[Inst[2]] then InstrPoint=InstrPoint + 1; else InstrPoint = Inst[3]; end;end; elseif Enum <= 12 then Stk[Inst[2]] = Inst[3]; elseif Enum > 13 then Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];else Stk[Inst[2]] = Stk[Inst[3]] + Inst[4];end; elseif Enum <= 16 then if Enum > 15 then local B;local A;Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];else Stk[Inst[2]]=Upvalues[Inst[3]];end; elseif Enum <= 17 then Stk[Inst[2]]=Upvalues[Inst[3]]; elseif Enum == 18 then local A;Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A]()
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
do return Stk[Inst[2]] end
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;else Stk[Inst[2]]=Stk[Inst[3]]-Stk[Inst[4]];end; elseif Enum <= 29 then if Enum <= 24 then if Enum <= 21 then if Enum == 20 then Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];else local A;
A= Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
do return Stk[Inst[2]] end
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 22 then 
local A = Inst[2];
local T = Stk[A];
for Idx = A + 1, Inst[3] do 
	Insert(T, Stk[Idx])
end; elseif Enum > 23 then Stk[Inst[2]][Inst[3]] = Inst[4];else if (Inst[2] <= Stk[Inst[4]]) then InstrPoint=Inst[3]; else InstrPoint=InstrPoint+1; end;end; elseif Enum <= 26 then if Enum > 25 then Stk[Inst[2]]=Stk[Inst[3]]*Stk[Inst[4]];else local A=Inst[2];local B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];end; elseif Enum <= 27 then if(Stk[Inst[2]] ~= Inst[4]) then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end; elseif Enum > 28 then 
local A = Inst[2]
Stk[A] = Stk[A]()
else Stk[Inst[2]]=Env[Inst[3]];end; elseif Enum <= 34 then if Enum <= 31 then if Enum > 30 then Stk[Inst[2]]=Stk[Inst[3]];else 
local A = Inst[2];
local T = Stk[A];
for Idx = A + 1, Inst[3] do 
	Insert(T, Stk[Idx])
end;end; elseif Enum <= 32 then Stk[Inst[2]]=(Inst[3]~=0);InstrPoint=InstrPoint+1; elseif Enum > 33 then Stk[Inst[2]][Inst[3]] = Inst[4];else local B;local A;Stk[Inst[2]]={};InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]={};InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Inst[4];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Inst[4];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Env[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];end; elseif Enum <= 37 then if Enum <= 35 then for Idx=Inst[2],Inst[3] do Stk[Idx]=nil;end; elseif Enum > 36 then if(Stk[Inst[2]] == Inst[4])then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;else if(Stk[Inst[2]] ~= Inst[4]) then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;end; elseif Enum <= 38 then local NewProto=Proto[Inst[3]];local NewUvals;local Indexes={};NewUvals=Setmetatable({},{__index=function(_,Key)local Val=Indexes[Key];return Val[1][Val[2]];end,__newindex=function(_,Key,Value)local Val=Indexes[Key] Val[1][Val[2]]=Value;end;});for Idx=1,Inst[4] do InstrPoint=InstrPoint+1;local Mvm=Instr[InstrPoint];if Mvm[1]==62 then Indexes[Idx-1]={Stk,Mvm[3]};else Indexes[Idx-1]={Upvalues,Mvm[3]};end;Lupvals[#Lupvals+1]=Indexes;end;Stk[Inst[2]]=Wrap(NewProto,NewUvals,Env); elseif Enum == 39 then local B;local A;Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]; 
do return Unpack(Stk, A, Top) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;else 
local A = Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;end; elseif Enum <= 61 then if Enum <= 50 then if Enum <= 45 then if Enum <= 42 then if Enum > 41 then if(Stk[Inst[2]] == Inst[4])then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;else InstrPoint=Inst[3];end; elseif Enum <= 43 then Stk[Inst[2]]=Stk[Inst[3]][Inst[4]]; elseif Enum == 44 then Stk[Inst[2]]=Stk[Inst[3]]*Stk[Inst[4]];else do return end;end; elseif Enum <= 47 then if Enum > 46 then Stk[Inst[2]]=(Inst[3]~=0);else Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]]-Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
do return Stk[Inst[2]] end
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 48 then 
local A = Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
 elseif Enum == 49 then if not Stk[Inst[2]] then InstrPoint=InstrPoint+1;else InstrPoint=Inst[3];end;else if Stk[Inst[2]] then InstrPoint=InstrPoint + 1; else InstrPoint = Inst[3]; end;end; elseif Enum <= 55 then if Enum <= 52 then if Enum > 51 then 
do return Stk[Inst[2]] end
else 
local A = Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
end; elseif Enum <= 53 then Stk[Inst[2]]={}; elseif Enum == 54 then if Stk[Inst[2]] then InstrPoint=InstrPoint + 1; else InstrPoint = Inst[3]; end;else 
local A = Inst[2]; 
do return Unpack(Stk, A, Top) end;end; elseif Enum <= 58 then if Enum <= 56 then 
local A = Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
 elseif Enum > 57 then 
local A = Inst[2]
Stk[A] = Stk[A]()
else 
local A = Inst[2]; 
do return Unpack(Stk, A, Top) end;end; elseif Enum <= 59 then Stk[Inst[2]] = Inst[3]; elseif Enum > 60 then Stk[Inst[2]]=(Inst[3]~=0);else 
local A = Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
end; elseif Enum <= 71 then if Enum <= 66 then if Enum <= 63 then if Enum == 62 then Stk[Inst[2]]=Stk[Inst[3]];else Stk[Inst[2]] = Stk[Inst[3]] % Inst[4];end; elseif Enum <= 64 then local B;local A;A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A] = Stk[A](Stk[A + 1]) 
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]; 
do return Unpack(Stk, A, Top) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end; elseif Enum > 65 then 
local A = Inst[2]
Stk[A](Stk[A + 1])
else local B;local A;A=Inst[2];B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
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
do return Unpack(Stk, A, Top) end;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 68 then if Enum > 67 then if (Inst[2] <= Stk[Inst[4]]) then InstrPoint=Inst[3]; else InstrPoint=InstrPoint+1; end;else local A;local K;local B;Stk[Inst[2]] = Inst[3];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];B=Inst[3];K=Stk[B] for Idx=B+1,Inst[4] do K=K..Stk[Idx];end;Stk[Inst[2]]=K;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];
A= Inst[2]
Stk[A](Unpack(Stk, A + 1, Inst[3]))
InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;end; elseif Enum <= 69 then local B=Inst[3];local K=Stk[B] for Idx=B+1,Inst[4] do K=K..Stk[Idx];end;Stk[Inst[2]]=K; elseif Enum == 70 then 
local A = Inst[2];
do return Stk[A](Unpack(Stk, A + 1, Inst[3])) end;else Stk[Inst[2]]=Stk[Inst[3]]-Stk[Inst[4]];end; elseif Enum <= 76 then if Enum <= 73 then if Enum == 72 then local A=Inst[2];local B=Stk[Inst[3]];Stk[A+1]=B;Stk[A]=B[Inst[4]];else do return end;end; elseif Enum <= 74 then for Idx=Inst[2],Inst[3] do Stk[Idx]=nil;end; elseif Enum > 75 then 
local A = Inst[2]
Stk[A] = Stk[A](Unpack(Stk, A + 1, Inst[3])) 
else local B=Inst[3];local K=Stk[B] for Idx=B+1,Inst[4] do K=K..Stk[Idx];end;Stk[Inst[2]]=K;end; elseif Enum <= 79 then if Enum <= 77 then Stk[Inst[2]]=Wrap(Proto[Inst[3]],nil,Env); elseif Enum > 78 then Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];else Stk[Inst[2]]=(Inst[3]~=0);InstrPoint=InstrPoint+1;end; elseif Enum <= 80 then Stk[Inst[2]]=Wrap(Proto[Inst[3]],nil,Env); elseif Enum == 81 then local NewProto=Proto[Inst[3]];local NewUvals;local Indexes={};NewUvals=Setmetatable({},{__index=function(_,Key)local Val=Indexes[Key];return Val[1][Val[2]];end,__newindex=function(_,Key,Value)local Val=Indexes[Key] Val[1][Val[2]]=Value;end;});for Idx=1,Inst[4] do InstrPoint=InstrPoint+1;local Mvm=Instr[InstrPoint];if Mvm[1]==62 then Indexes[Idx-1]={Stk,Mvm[3]};else Indexes[Idx-1]={Upvalues,Mvm[3]};end;Lupvals[#Lupvals+1]=Indexes;end;Stk[Inst[2]]=Wrap(NewProto,NewUvals,Env);else Stk[Inst[2]][Inst[3]] = Stk[Inst[4]];end;
			InstrPoint	= InstrPoint + 1;
		end;
    end;
end;	
return Wrap(Deserialize(), {}, GetFEnv())();
