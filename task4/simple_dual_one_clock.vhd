-- Simple Dual-Port Block RAM with One Clock
-- Correct Modelization with a Shared Variable
-- File:simple_dual_one_clock.vhd

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity simple_dual_one_clock is
 port(
  clk   : in  std_logic;
  ena   : in  std_logic;
  enb   : in  std_logic;
  wea   : in  std_logic;
<<<<<<< HEAD
  addra : in  std_logic_vector(8 downto 0);
  addrb : in  std_logic_vector(8 downto 0);
=======
  addra : in  std_logic_vector(9 downto 0);
  addrb : in  std_logic_vector(9 downto 0);
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338
  dia   : in  std_logic_vector(31 downto 0);
  dob   : out std_logic_vector(31 downto 0)
 );
end simple_dual_one_clock;

architecture syn of simple_dual_one_clock is
<<<<<<< HEAD
 type ram_type is array ((2 ** 9) downto 0) of std_logic_vector(31 downto 0);
=======
 type ram_type is array (1023 downto 0) of std_logic_vector(31 downto 0);
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338
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