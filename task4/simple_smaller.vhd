-- Simple Dual-Port Block RAM with One Clock but with smaller address space
-- Correct Modelization with a Shared Variable
-- File:simple_dual_one_clock.vhd

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity simple_smaller is
 port(
  clk   : in  std_logic;
  ena   : in  std_logic;
  enb   : in  std_logic;
  wea   : in  std_logic;
  addra : in  std_logic_vector(7 downto 0);
  addrb : in  std_logic_vector(7 downto 0);
  dia   : in  std_logic_vector(31 downto 0);
  dob   : out std_logic_vector(31 downto 0)
 );
end simple_smaller;

architecture syn of simple_smaller is
 type ram_type is array ((2 ** 8) downto 0) of std_logic_vector(31 downto 0);
 shared variable RAM : ram_type;
begin
 process(clk)
 begin -- Determine writing 
  if clk'event and clk = '1' then
   if ena = '1' then
    if wea = '1' then
     RAM(conv_integer(addra)) := dia;
    end if;
   end if;
  end if;
 end process;

 process(clk)
 begin -- Determine reading
  if clk'event and clk = '1' then
   if enb = '1' then
    dob <= RAM(conv_integer(addrb));
   end if;
  end if;
 end process;

end syn;