-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 2.
--             :
--  Developers :  Andras - s??????@student.dtu.dk 
--             :  Rene - s??????@student.dtu.dk
--             :
--  Purpose    :  This design contains an entity for the accelerator that must be built
--             :  in task two of the Edge Detection design project. It contains an
--             :  architecture skeleton for the entity as well.
--             :
--  Revision   :  1.0   ??-??-??     Final version
--             :
--
-- -----------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The entity for task two. Notice the additional signals for the memory.
-- Reset is active high.
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
        dataR  : in  word_t;            -- The data bus for reading.
        dataW  : out word_t;            -- The data bus for writing.
        en     : out bit_t;             -- Request signal for data.
        we     : out bit_t;             -- Read/Write signal for data.
        start  : in  bit_t;
        finish : out bit_t
    );
end acc;

--------------------------------------------------------------------------------
-- The description of the accelerator.
--------------------------------------------------------------------------------

architecture rtl of acc is

    -- All internal signals are defined here
    type state_type is (
        I1, I2, I3, I4, I5, I6, I7,
        S0, S1, S2, S3, S4, S_WRITE, F
    ); -- Added S_WRITE state

    signal state, next_state          : state_type;
    signal row_1, next_row_1          : std_logic_vector(63 downto 0) := (others => '0');
    signal row_2, next_row_2          : std_logic_vector(71 downto 0) := (others => '0');
    signal row_3, next_row_3          : std_logic_vector(79 downto 0) := (others => '0');
    signal curr_write_buff, next_write_buff : word_t;
    
    
    signal dx, dy                     : signed(11 downto 0);   -- 12-bit signed for intermediate calculations
    signal abs_dx, abs_dy             : std_logic_vector(11 downto 0); -- 12-bit unsigned for absolute values
    signal sobel_sum                  : std_logic_vector(11 downto 0); -- 12-bit unsigned for sum of gradients
    signal result_full                : std_logic_vector(11 downto 0);
    signal result                     : std_logic_vector(7 downto 0);  -- 8-bit output
    

    -- Control signals
    signal curr_en           : std_logic;
    signal curr_we           : std_logic;
    signal addr_read  : halfword_t;
    signal addr_write : halfword_t;
    signal curr_addr                        : halfword_t;
    signal curr_addr_ptr, next_addr_ptr     : std_logic_vector(15 downto 0);


    component three_dual_one_clock
        port (
            clk    : in  std_logic;
            ena    : in  std_logic;                    
            enb    : in  std_logic;                    
            wea    : in  std_logic;                    
            addra  : in  std_logic_vector(9 downto 0); 
            addrb  : in  std_logic_vector(9 downto 0); 
            tag_offset   : in std_logic_vector(1 downto 0);
            dia1    : in  std_logic_vector(15 downto 0); 
            dia2    : in  std_logic_vector(15 downto 0); 
            dia3    : in  std_logic_vector(15 downto 0); 
            dob1   : out std_logic_vector(15 downto 0); 
            dob2   : out std_logic_vector(15 downto 0); 
            dob3   : out std_logic_vector(15 downto 0)  
        );
    end component;
    
begin

    -- Output assignments
    we   <= curr_we;
    en   <= curr_en;
    addr <= curr_addr;

    -- Combinational process for Dx, Dy, and Sobel filter computation
    cl_sobel: process(row_1, row_2, row_3, dx, dy, abs_dx, abs_dy, sobel_sum)    
    
    begin
    -- Calculate horizontal gradient (Dx)
    dx <= signed(x"0" & row_1(7 downto 0))                  -- s13
               - signed(x"0" & row_1(23 downto 16))              -- s11
               + signed(unsigned(x"0" & row_2(7 downto 0)) sll 1) -- s23 * 2 with unsigned shift
               - signed(unsigned(x"0" & row_2(23 downto 16)) sll 1) -- s21 * 2 with unsigned shift
               + signed(x"0" & row_3(7 downto 0))                -- s33
               - signed(x"0" & row_3(23 downto 16));             -- s31
   
    
    -- Calculate vertical gradient (Dy)
    dy <= signed(x"0" & row_1(23 downto 16))                -- s11
               - signed(x"0" & row_3(23 downto 16))              -- s31
               + signed(unsigned(x"0" & row_1(15 downto 8)) sll 1) -- s12 * 2 with unsigned shift
               - signed(unsigned(x"0" & row_3(15 downto 8)) sll 1) -- s32 * 2 with unsigned shift
               + signed(x"0" & row_1(7 downto 0))                -- s13
               - signed(x"0" & row_3(7 downto 0));               -- s33

        -- Compute absolute values and sum them
   abs_dx <= std_logic_vector(abs(dx));
   abs_dy <= std_logic_vector(abs(dy));

   -- Sum of absolute gradients
   sobel_sum <= std_logic_vector(unsigned(abs_dx) + unsigned(abs_dy));
        
   result <= sobel_sum(10 downto 3);  -- Use the top 8 bits for scaling
   end process cl_sobel;



    cl_addr: process(curr_en, curr_we, addr_write, addr_read)    
    begin
    -- Update current address based on we and en signals
    if (curr_en = '1' and curr_we = '1') then
        curr_addr <= addr_write;
    elsif (curr_en = '1' and curr_we = '0') then
        curr_addr <= addr_read;
    else
        curr_addr <= (others => '0');
    end if;
    end process cl_addr;




    -- Combinational logic for state transitions and output logic
    cl_states: process(
        start, state, curr_addr_ptr, curr_write_buff,
        addr_write, addr_read, row_1, row_2, row_3, result
    )
        constant row_2_offs  : integer := 88;
        constant row_3_offs  : integer := 176;
        constant write_offs  : integer := 25344;
    begin

        -- Default assignments
        next_write_buff <= curr_write_buff;
        next_state      <= state;
        next_addr_ptr   <= curr_addr_ptr;
        next_row_1      <= row_1;
        next_row_2      <= row_2;
        next_row_3      <= row_3;
        finish          <= '0';
        dataW <= (others => '0');
        
        case state is
            -- S0: Idle
            when S0 =>
                curr_en         <= '1';
                curr_we         <= '0';
                next_write_buff <= (others => '0');
                next_addr_ptr   <= (others => '0');
                addr_read  <= (others => '0');
                addr_write <= (others => '0');
                if start = '1' then
                    next_state      <= I1;
                end if;

            -- Initialization states (I1 to I7)
            when I1 =>
                curr_en         <= '1';
                curr_we         <= '0';
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs);
                next_row_1(31 downto 0) <= dataR;
                next_state      <= I2;

            when I2 =>
                curr_en         <= '1';
                curr_we         <= '0';
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                next_row_2(31 downto 0) <= dataR;
                next_addr_ptr   <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                next_state      <= I3;

            when I3 =>
                curr_en         <= '1';
                curr_we         <= '0';
                addr_read  <= curr_addr_ptr;
                next_row_3(31 downto 0) <= dataR;
                next_state      <= I4;

            when I4 =>
                curr_en         <= '1';
                curr_we         <= '0';
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs);
                next_row_1(63 downto 32) <= dataR;
                next_state      <= I5;

            when I5 =>
                curr_en         <= '1';
                curr_we         <= '0';
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                next_row_2(63 downto 32) <= dataR;
                next_addr_ptr   <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                next_state      <= I6;

            when I6 =>
                curr_en         <= '1';
                curr_we         <= '0';
                next_row_3(63 downto 32) <= dataR;
                next_state      <= I7;

            when I7 =>
                curr_en         <= '1';
                curr_we         <= '0';
                next_state      <= S1;

            -- Processing states (S1 to S4), assembling write buffer
            when S1 =>
                -- Check for end of processing
                if (unsigned(curr_addr_ptr) + row_3_offs = write_offs + 2) then
                    next_state <= F;
                else
                    curr_en        <= '1';
                    curr_we        <= '0';
                    addr_read <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                    -- Shift rows
                    next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                    next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                    next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                    -- Save result into write buffer (delayed by one state)
                    next_write_buff(15 downto 8) <= result;
                    next_state <= S2;
                end if;

            when S2 =>
                curr_en        <= '1';
                curr_we        <= '0';
                addr_read <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs);
                -- Shift rows
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                next_row_3(79 downto 48) <= dataR;
                next_write_buff(23 downto 16) <= result;
                next_state <= S3;

            when S3 =>
                curr_en        <= '1';
                curr_we        <= '0';
                addr_read <= curr_addr_ptr;
                -- Shift rows
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                next_row_2(71 downto 40) <= dataR;
                if ((unsigned(curr_addr_ptr)-1) mod 88) = 0 then --the last pixel of each row to write
                    next_write_buff(31 downto 24) <= "00000000";
                else
                    next_write_buff(31 downto 24) <= result;
                end if;
                next_state <= S4;

            when S4 =>
                -- Shift rows
                curr_en        <= '1';
                curr_we        <= '1';
                next_row_1 <= std_logic_vector(unsigned(row_1) srl 8);
                next_row_2 <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3 <= std_logic_vector(unsigned(row_3) srl 8);
                next_row_1(63 downto 32) <= dataR;
                -- Save result into write buffer
                if ((unsigned(curr_addr_ptr)-1) mod 88) = 0 then
                    next_write_buff(7 downto 0) <= "00000000";
                else
                    next_write_buff(7 downto 0) <= result;
                end if;
                addr_write <= std_logic_vector(unsigned(curr_addr_ptr) + write_offs - 2 + row_2_offs);
                next_state <= S_WRITE;
                dataW <= curr_write_buff;

            when S_WRITE =>
                curr_we        <= '0';
                curr_en        <= '1';
                -- After write, prepare for next set of pixels
                next_addr_ptr  <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                next_state     <= S1;

            -- Finish state
            when F =>
                finish <= '1';
                if start = '1' then
                    next_state <= F;
                else
                    next_state <= S0;
                end if;

            -- Default case
            when others =>
                next_state <= S0;
        end case;

    end process cl_states;

    -- Sequential process to update state and signals
    myprocess: process(clk, curr_en, curr_we)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                -- Initialize signals on reset
                state           <= S0;
                curr_addr_ptr   <= (others => '0');
                curr_write_buff <= (others => '0');
                row_1           <= (others => '0');
                row_2           <= (others => '0');
                row_3           <= (others => '0');
            else
                -- Update current state and signals
                state           <= next_state;
                curr_addr_ptr   <= next_addr_ptr;
                row_1           <= next_row_1;
                row_2           <= next_row_2;
                row_3           <= next_row_3;
                curr_write_buff <= next_write_buff;
                
            end if;
        end if;
    end process myprocess;

end rtl;