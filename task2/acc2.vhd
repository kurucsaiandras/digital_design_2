-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 2.
--             :
--  Developers :  YOUR NAME HERE - s??????@student.dtu.dk
--             :  YOUR NAME HERE - s??????@student.dtu.dk
--             :
--  Purpose    :  This design contains an entity for the accelerator that must be build
--             :  in task two of the Edge Detection design project. It contains an
--             :  architecture skeleton for the entity as well.
--             :
--  Revision   :  1.0   ??-??-??     Final version
--             :
--
-- -----------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The entity for task two. Notice the additional signals for the memory.
-- reset is active high.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.types.all;

entity acc is
    port(
        clk    : in  bit_t;             -- The clock.
        reset  : in  bit_t;             -- The reset signal. Active high.
        addr   : out halfword_t;        -- Address bus for data.
        dataR  : in  word_t;            -- The data bus.
        dataW  : out word_t;            -- The data bus.
        en     : out bit_t;             -- Request signal for data.
        we     : out bit_t;             -- Read/Write signal for data.
        start  : in  bit_t;
        finish : out bit_t
    );
end acc;

--------------------------------------------------------------------------------
-- The desription of the accelerator.
--------------------------------------------------------------------------------

architecture rtl of acc is

-- All internal signals are defined here
  type state_type is (I1, I2, I3, I4, I5, I6, I7, S0, S1, S2, S3, S4, F); -- States representing FSM states
  
  signal addr_ptr, next_addr_ptr: halfword_t;
  signal state, next_state: state_type;
  signal row_1, next_row_1: std_logic_vector(63 downto 0) := (others => '0');
  signal row_2, next_row_2: std_logic_vector(71 downto 0) := (others => '0');
  signal row_3, next_row_3: std_logic_vector(79 downto 0) := (others => '0');
  signal write_buff, next_write_buff: word_t;
  signal result: std_logic_vector(7 downto 0);
  signal dx, dy: integer range -1024 to 1023;  -- Increase range to avoid overflow during intermediate sums
  signal abs_dx, abs_dy, sobel_sum: integer range 0 to 2047;  -- Store the absolute values and their sum
  signal result_int: integer range 0 to 255;   -- Store the final result after division

begin
    -- Combinational process for Dx, Dy, and Sobel filter computation
    cl_sobel: process(row_1, row_2, row_3, dx, dy, abs_dx, abs_dy, sobel_sum, result_int)
    begin
        -- Calculate horizontal gradient (Dx)
        dx <= to_integer(signed('0' & row_1(7 downto 0)))    -- s13
             - to_integer(signed('0' & row_1(23 downto 16))) -- s11
             + 2 * (to_integer(signed('0' & row_2(7 downto 0)))  -- s23
             - to_integer(signed('0' & row_2(23 downto 16))))    -- s21
             + to_integer(signed('0' & row_3(7 downto 0)))    -- s33
             - to_integer(signed('0' & row_3(23 downto 16))); -- s31
        
        -- Calculate vertical gradient (Dy)
        dy <= to_integer(signed('0' & row_1(23 downto 16))) -- s11
             - to_integer(signed('0' & row_3(23 downto 16))) -- s31
             + 2 * (to_integer(signed('0' & row_1(15 downto 8))) -- s12
             - to_integer(signed('0' & row_3(15 downto 8))))    -- s32
             + to_integer(signed('0' & row_1(7 downto 0)))       -- s13
             - to_integer(signed('0' & row_3(7 downto 0)));      -- s33

        -- Compute absolute values and sum them
        abs_dx <= abs(dx);
        abs_dy <= abs(dy);
        sobel_sum <= abs_dx + abs_dy;

        -- Divide the result by 6 and clamp to 8 bits (range 0-255)
        result_int <= sobel_sum / 6;

        -- Convert the result to std_logic_vector (8 bits)
        result <= std_logic_vector(to_unsigned(result_int, 8));
    end process cl_sobel;
    
    -- Combinational logic for state transitions and output logic
    cl_states: process(reset, dataR, start, state, addr_ptr, row_1, row_2, row_3, write_buff, result)
    constant row_2_offs: integer := 88;
    constant row_3_offs: integer := 176;
    constant write_offs: integer := 25344;
    begin
        next_write_buff <= write_buff;
        next_state <= state;
        next_addr_ptr <= addr_ptr;
        next_row_1 <= row_1;
        next_row_2 <= row_2;
        next_row_3 <= row_3;
        en <= '0';
        we <= '0';
        finish <= '0';
        dataW <= (others => '0');
        addr <= (others => '0');
        
    case state is
        -- S0: Idle
        when S0 =>
            if start = '1' then
                next_state <= I1;
            end if;
        -- I1: Init 1, Address for 1st row
        when I1 =>
            en <= '1';
            we <= '0';
            addr <= addr_ptr;
            next_state <= I2;
        -- I2: Init 2, Read 1st row, Address for 2nd row
        when I2 =>
            en <= '1';
            we <= '0';
            addr <= std_logic_vector(unsigned(addr_ptr) + row_2_offs); -- offset for the 2nd row;
            next_row_1(31 downto 0) <= dataR;
            next_state <= I3;
        -- I3: Init 3, Read 2nd row, Address for 3rd row
        when I3 =>
            en <= '1';
            we <= '0';
            addr <= std_logic_vector(unsigned(addr_ptr) + row_3_offs); -- offset for the 3rd row;
            next_row_2(31 downto 0) <= dataR;
            next_addr_ptr <= std_logic_vector(unsigned(addr_ptr) + 1);
            next_state <= I4;
        -- I4: Init 4, Read 3rd row, Address for 1st row 
        when I4 =>
            en <= '1';
            we <= '0';
            addr <= addr_ptr;
            next_row_3(31 downto 0) <= dataR;
            next_state <= I5;
        -- I5: Init 5, Read 1st row, Address for 2nd row 
        when I5 =>
            en <= '1';
            we <= '0';
            addr <= std_logic_vector(unsigned(addr_ptr) + row_2_offs); -- offset for the 2nd row;
            next_row_1(63 downto 32) <= dataR;
            next_state <= I6;
        -- I6: Init 6, Read 2nd row, Address for 3rd row
        when I6 =>
            en <= '1';
            we <= '0';
            addr <= std_logic_vector(unsigned(addr_ptr) + row_3_offs); -- offset for the 3rd row;
            next_row_2(63 downto 32) <= dataR;
            next_addr_ptr <= std_logic_vector(unsigned(addr_ptr) + 1);
            next_state <= I7;
        -- I7: Init 7, Read 3rd row
        when I7 =>
            next_row_3(63 downto 32) <= dataR;
            next_state <= S1;
        
        -- S1: Read row_3, save pixel_2
        when S1 =>
            if (unsigned(addr_ptr) + row_3_offs = write_offs - 1) then -- if we are at the end of the picture
                next_state <= F;
                next_addr_ptr <= (others => '0');
            else
                en <= '1';
                we <= '0';
                addr <= std_logic_vector(unsigned(addr_ptr) + row_3_offs); -- offset for the 3rd row;
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                next_write_buff(15 downto 8) <= result;
                next_state <= S2;
            end if;
        -- S2: Read row_2, save row_3, save pixel_3
        when S2 =>
            en <= '1';
            we <= '0';
            addr <= std_logic_vector(unsigned(addr_ptr) + row_2_offs); -- offset for the 2nd row;
            next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
            next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
            next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
            next_row_3(79 downto 48) <= dataR;
            next_write_buff(23 downto 16) <= result;
            next_state <= S3;
        -- S3: Read row_1, save row_2, save pixel_4
        when S3 =>
            en <= '1';
            we <= '0';
            addr <= addr_ptr; -- 1st row
            next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
            next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
            next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
            next_row_2(71 downto 40) <= dataR;
            next_write_buff(31 downto 24) <= result;
            next_state <= S4;
        -- S4: Write results, save row_1, save pixel_1
        when S4 =>
            en <= '1';
            we <= '1';
            addr <= std_logic_vector(unsigned(addr_ptr) + write_offs - 2 + row_2_offs); -- reading is ahead by 2 words and we start with the second row
            next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
            next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
            next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
            next_row_1(63 downto 32) <= dataR;
            dataW <= write_buff;
            next_write_buff(7 downto 0) <= result;
            next_addr_ptr <= std_logic_vector(unsigned(addr_ptr) + 1);
            next_state <= S1;

        -- F: Finish
        when F =>
            finish <= '1';
            next_state <= S0;
        -- Default case
        when others =>
            next_state <= S0;
    end case;
            
    end process cl_states;

    myprocess : process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                state <= S0;
                addr_ptr <= (others => '0');
                write_buff <= (others => '0');
            else
                state <= next_state;
                addr_ptr <= next_addr_ptr;
                row_1 <= next_row_1;
                row_2 <= next_row_2;
                row_3 <= next_row_3;
                write_buff <= next_write_buff;
            end if;
        end if;
    end process myprocess;

end rtl;
