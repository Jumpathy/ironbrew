local d=string.byte;local r=string.char;local c=string.sub;local K=table.concat;local u=table.insert;local b=math.ldexp;local s=getfenv or function()return _ENV end;local l=setmetatable;local h=select;local f=unpack or table.unpack;local i=tonumber;local function H(d)local e,n,a="","",{}local o=256;local t={}for l=0,o-1 do t[l]=r(l)end;local l=1;local function f()local e=i(c(d,l,l),36)l=l+1;local n=i(c(d,l,l+e-1),36)l=l+e;return n end;e=r(f())a[1]=e;while l<#d do local l=f()if t[l]then n=t[l]else n=e..c(e,1,1)end;t[o]=e..c(n,1,1)a[#a+1],e,o=n,n,o+1 end;return table.concat(a)end;local i=H('24C24427524524127524426C26E27126Y26824524527926T27927K23K25W27G27926V24524027926Y26P26K27F27H27526R27O27527028124427124423K27923Q23P27927Y24423Q27624424628927528I24728J24428M27S27K27928M27H28F27927828R27H28M27528V28I24228N28M24328N27S27427924F24427H27S27Y23R28H29I27524024A27928B28U27S27828D27929429027K29728Q27527827829727H24229727524929D29I29L2792A727Y29428G28R244');local o=bit and bit.bxor or function(l,e)local n,o=1,0
while l>0 and e>0 do
local c,a=l%2,e%2
if c~=a then o=o+n end
l,e,n=(l-c)/2,(e-a)/2,n*2
end
if l<e then l=e end
while l>0 do
local e=l%2
if e>0 then o=o+n end
l,n=(l-e)/2,n*2
end
return o
end
local function n(e,l,n)if n then
local l=(e/2^(l-1))%2^((n-1)-(l-1)+1);return l-l%1;else
local l=2^(l-1);return(e%(l+l)>=l)and 1 or 0;end;end;local l=1;local function e()local n,c,a,e=d(i,l,l+3);n=o(n,148)c=o(c,148)a=o(a,148)e=o(e,148)l=l+4;return(e*16777216)+(a*65536)+(c*256)+n;end;local function t()local e=o(d(i,l,l),148);l=l+1;return e;end;local function a()local e,n=d(i,l,l+2);e=o(e,148)n=o(n,148)l=l+2;return(n*256)+e;end;local function H()local l=e();local e=e();local c=1;local o=(n(e,1,20)*(2^32))+l;local l=n(e,21,31);local e=((-1)^n(e,32));if(l==0)then
if(o==0)then
return e*0;else
l=1;c=0;end;elseif(l==2047)then
return(o==0)and(e*(1/0))or(e*(0/0));end;return b(e,l-1023)*(c+(o/(2^52)));end;local b=e;local function Y(e)local n;if(not e)then
e=b();if(e==0)then
return'';end;end;n=c(i,l,l+e-1);l=l+e;local e={}for l=1,#n do
e[l]=r(o(d(c(n,l,l)),148))end
return K(e);end;local l=e;local function b(...)return{...},h('#',...)end
local function i()local r={};local f={};local l={};local d={r,f,nil,l};local l=e()local c={}for n=1,l do
local e=t();local l;if(e==3)then l=(t()~=0);elseif(e==0)then l=H();elseif(e==1)then l=Y();end;c[n]=l;end;d[3]=t();for d=1,e()do
local l=t();if(n(l,1,1)==0)then
local o=n(l,2,3);local t=n(l,4,6);local l={a(),a(),nil,nil};if(o==0)then
l[3]=a();l[4]=a();elseif(o==1)then
l[3]=e();elseif(o==2)then
l[3]=e()-(2^16)elseif(o==3)then
l[3]=e()-(2^16)l[4]=a();end;if(n(t,1,1)==1)then l[2]=c[l[2]]end
if(n(t,2,2)==1)then l[3]=c[l[3]]end
if(n(t,3,3)==1)then l[4]=c[l[4]]end
r[d]=l;end
end;for l=1,e()do f[l-1]=i();end;return d;end;local function r(l,e,a)local e=l[1];local n=l[2];local l=l[3];return function(...)local c=e;local e=n;local o=l;local l=b
local n=1;local l=-1;local i={};local d={...};local t=h('#',...)-1;local l={};local e={};for l=0,t do
if(l>=o)then
i[l-o]=d[l+1];else
e[l]=d[l+1];end;end;local l=t-o+1
local l;local o;while true do
l=c[n];o=l[1];if o<=9 then if o<=4 then if o<=1 then if o>0 then n=l[3];else e[l[2]]=e[l[3]];end;elseif o<=2 then do return end;elseif o==3 then
local n=l[2];local o=e[n];for l=n+1,l[3]do
u(o,e[l])end;else e[l[2]]=a[l[3]];end;elseif o<=6 then if o==5 then
local n=l[2]e[n](f(e,n+1,l[3]))else do return end;end;elseif o<=7 then
local o=l[2];local a=l[4];local c=o+2
local o={e[o](e[o+1],e[c])};for l=1,a do
e[c+l]=o[l];end;local o=o[1]if o then
e[c]=o
n=l[3];else
n=n+1;end;elseif o>8 then
local c=l[2];local a=l[4];local o=c+2
local c={e[c](e[c+1],e[o])};for l=1,a do
e[o+l]=c[l];end;local c=c[1]if c then
e[o]=c
n=l[3];else
n=n+1;end;else e[l[2]]=l[3];end;elseif o<=14 then if o<=11 then if o==10 then e[l[2]]=l[3];else
local n=l[2];local o=e[n];for l=n+1,l[3]do
u(o,e[l])end;end;elseif o<=12 then for l=l[2],l[3]do e[l]=nil;end;elseif o>13 then n=l[3];else e[l[2]]=e[l[3]];end;elseif o<=17 then if o<=15 then e[l[2]]={};elseif o==16 then e[l[2]]=a[l[3]];else local o;e[l[2]]=a[l[3]];n=n+1;l=c[n];e[l[2]]=l[3];n=n+1;l=c[n];e[l[2]]=l[3];n=n+1;l=c[n];e[l[2]]=l[3];n=n+1;l=c[n];o=l[2]e[o](f(e,o+1,l[3]))n=n+1;l=c[n];e[l[2]]=a[l[3]];n=n+1;l=c[n];e[l[2]]={};n=n+1;l=c[n];e[l[2]]=l[3];n=n+1;l=c[n];e[l[2]]=l[3];n=n+1;l=c[n];e[l[2]]=l[3];end;elseif o<=18 then e[l[2]]={};elseif o==19 then for l=l[2],l[3]do e[l]=nil;end;else
local n=l[2]e[n](f(e,n+1,l[3]))end;n=n+1;end;end;end;return r(i(),{},s())();