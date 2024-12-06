library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity three_dual_one_clock is
    port (
        clk    : in  std_logic;
        ena    : in  std_logic;                    -- Enable signal for all instances
        enb    : in  std_logic;                    -- Enable signal for all instances
        wea    : in  std_logic;                    -- Write enable for all instances
<<<<<<< HEAD
        addra  : in  std_logic_vector(6 downto 0); -- Address input
        addrb  : in  std_logic_vector(6 downto 0); -- Address input
=======
        addra  : in  std_logic_vector(7 downto 0); -- Address input
        addrb  : in  std_logic_vector(7 downto 0); -- Address input
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338
        tag_offset   : in std_logic_vector(1 downto 0); -- The tag offset is in regards to the write. Write on the first row is 0, second row is 1, third row is 2 
        dia   : in  std_logic_vector(31 downto 0); -- 32-bit Data input for write
        dob1   : out std_logic_vector(31 downto 0); -- 32-bit Data output from instance 1
        dob2   : out std_logic_vector(31 downto 0); -- 32-bit Data output from instance 2
        dob3   : out std_logic_vector(31 downto 0)  -- 32-bit Data output from instance 3
    );
end three_dual_one_clock;

architecture rtl of three_dual_one_clock is

    -- Signals to connect to each block RAM instance
    signal ram_dob1 : std_logic_vector(31 downto 0);
    signal ram_dob2 : std_logic_vector(31 downto 0);
    signal ram_dob3 : std_logic_vector(31 downto 0);

    -- Declare the 32-bit version of the simple dual-port RAM component
    component simple_dual_one_clock
        port(
            clk   : in  std_logic;
            ena   : in  std_logic;
            enb   : in  std_logic;
            wea   : in  std_logic;
<<<<<<< HEAD
            addra : in  std_logic_vector(8 downto 0); -- Write address
            addrb : in  std_logic_vector(8 downto 0); -- Read address
=======
            addra : in  std_logic_vector(9 downto 0); -- Write address
            addrb : in  std_logic_vector(9 downto 0); -- Read address
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338
            dia   : in  std_logic_vector(31 downto 0); -- Updated to 32-bit
            dob   : out std_logic_vector(31 downto 0)  -- Updated to 32-bit
        );
    end component;

<<<<<<< HEAD
    signal addr_row1, addr_row2, addr_row3, addr_write : std_logic_vector(8 downto 0);
=======
    signal addr_row1, addr_row2, addr_row3, addr_write : std_logic_vector(9 downto 0);
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338

begin


<<<<<<< HEAD
cl_addr: process(addra, addrb, tag_offset)
    begin
        case tag_offset is
            when "00" => -- Writing to initial row 3
=======
    cl_addr: process(addra, addrb, tag_offset)
    begin
        case tag_offset is
            when "00" => -- Writing to initial row 3, if we initially at "00" 
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338
                addr_write <= '0' & '0' & addra;
                
                addr_row1 <= '1' & '0' & addrb;
                addr_row2 <= '0' & '1' & addrb;
                addr_row3 <= '0' & '0' & addrb; -- Not used at the same time as write
<<<<<<< HEAD

=======
    
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338
            when "01" => -- Writing to initial row 2
                addr_write <= '0' & '1' & addra;
                
                addr_row1 <= '0' & '0' & addrb;  -- Shifted offsets for cyclic behavior
                addr_row2 <= '1' & '0' & addrb;
                addr_row3 <= '0' & '1' & addrb;  -- Wraps back to "00"
<<<<<<< HEAD

=======
    
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338
            when "10" => -- Writing to initial row 1
                addr_write <= '1' & '0' & addra;
                
                addr_row1 <= '0' & '1' & addrb;  -- Shifted offsets for cyclic behavior
                addr_row2 <= '0' & '0' & addrb;  -- Wraps back to "00"
                addr_row3 <= '1' & '0' & addrb;  -- Continues cyclic order
<<<<<<< HEAD

            when "11" => -- Should not occur, but assign defaults
                addr_write <= (others => '0');
                addr_row1  <= (others => '0');
                addr_row2  <= (others => '0');
                addr_row3  <= (others => '0');

            when others =>
                addr_write <= (others => '0');
                addr_row1  <= (others => '0');
                addr_row2  <= (others => '0');
                addr_row3  <= (others => '0');


        end case;
 end process;

=======
                
            when "11" => -- Intention is to cycle hence never hit state "11"
                -- Do nothing
            
            when others =>
                -- Do nothing 
            
        end case;
    end process;
>>>>>>> 3b0b0320acfcd387c67f02977c8b174bab390338



    -- Instantiate the first 32-bit block RAM
    ram1: simple_dual_one_clock
        port map (
            clk   => clk,
            ena   => ena,
            enb   => enb,
            wea   => wea,
            addra => addr_write,
            addrb => addr_row1,
            dia   => dia,
            dob   => ram_dob1
        );

    -- Instantiate the second 32-bit block RAM
    ram2: simple_dual_one_clock
        port map (
            clk   => clk,
            ena   => ena,
            enb   => enb,
            wea   => wea,
            addra => addr_write,
            addrb => addr_row2,
            dia   => dia,
            dob   => ram_dob2
        );

    -- Instantiate the third 32-bit block RAM
    ram3: simple_dual_one_clock
        port map (
            clk   => clk,
            ena   => ena,
            enb   => enb,
            wea   => wea,
            addra => addr_write,
            addrb => addr_row3,
            dia   => dia,
            dob   => ram_dob3
        );

    -- Assign outputs
    dob1 <= ram_dob1;
    dob2 <= ram_dob2;
    dob3 <= ram_dob3;

end rtl;


