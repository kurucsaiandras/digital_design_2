-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 4.
--             :
--  Developers :  
--             :  Rene - s203880@student.dtu.dk
--             :
--  Purpose    :  This design contains an entity for the accelerator that must be built
--             :  in task two of the Edge Detection design project. It contains an
--             :  architecture skeleton for the entity as well.
--             :
--  Revision   :  1.0   06-12-2024     Final version
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
    
    component boundary_conditions
    port ( 
        row_1          : in std_logic_vector(31 downto 0); 
        row_2          : in std_logic_vector(31 downto 0);
        row_3          : in std_logic_vector(31 downto 0);
        en_flag        : in std_logic_vector(2 downto 0);
        result       : out std_logic_vector(7 downto 0);
        result2       : out std_logic_vector(7 downto 0);
        result3       : out std_logic_vector(7 downto 0);
        result4       : out std_logic_vector(7 downto 0)
    );
    end component;
    component three_dual_one_clock
        port (
            clk    : in  std_logic;
            ena    : in  std_logic;                    
            enb    : in  std_logic;                    
            wea    : in  std_logic;                    
            addra  : in  std_logic_vector(6 downto 0); 
            addrb  : in  std_logic_vector(6 downto 0); 
            tag_offset   : in std_logic_vector(1 downto 0);
            dia    : in  std_logic_vector(31 downto 0); 
            dob1   : out std_logic_vector(31 downto 0); 
            dob2   : out std_logic_vector(31 downto 0); 
            dob3   : out std_logic_vector(31 downto 0)  
        );
    end component;
    component simple_smaller
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
         end component;
        
        -- All internal signals are defined here
    type state_type is ( I1, I2, I3, I4, I5, I6, 
                         S1_init, S2_init, S3_init, S4_init,
                         S1_pre, S0, S1, S2, S_bound, F
    );
    
    
    
    signal state, next_state                            : state_type;
    signal curr_write_buff, next_write_buff             : word_t;
    signal row_1, next_row_1                            : std_logic_vector(63 downto 0) := (others => '0');
    signal row_2, next_row_2                            : std_logic_vector(71 downto 0) := (others => '0');
    signal row_3, next_row_3                            : std_logic_vector(79 downto 0) := (others => '0');
    

    signal result                                       : std_logic_vector(7 downto 0); -- Maybe add init.
    signal result2                                      : std_logic_vector(7 downto 0);  
    signal result3                                      : std_logic_vector(7 downto 0);
    signal result4                                      : std_logic_vector(7 downto 0);
    signal en_flag                                      : std_logic_vector(2 downto 0) := (others => '0');
    
    
    -- Control signals
    signal curr_en                                      : std_logic;
    signal curr_we                                      : std_logic;
    signal addr_read                                    : halfword_t;
    signal addr_write                                   : halfword_t;
    signal curr_addr                                    : halfword_t := (others => '0');
    signal curr_addr_ptr, next_addr_ptr                 : std_logic_vector(15 downto 0) := (others => '0');

    
    -- BRAM TO ACHIEVE SPEED UP. 3 ROWS ARE MEMORIZED AT A TIME
    signal bram_en_write                                : std_logic;
    signal bram_en_read                                 : std_logic;
    signal bram_we_write                                : std_logic;
    signal curr_bram_addr_ptr, next_bram_addr_ptr       : std_logic_vector(6 downto 0) := (others => '0');
    signal bram_addr_read                               : std_logic_vector(6 downto 0);
    signal bram_addr_write                              : std_logic_vector(6 downto 0);
    signal tag_offset                                   : std_logic_vector(1 downto 0)   := (others => '0');
    signal bram_read_row1                               : std_logic_vector(31 downto 0);
    signal bram_read_row2                               : std_logic_vector(31 downto 0);
    signal bram_read_row3                               : std_logic_vector(31 downto 0);
    signal bram_write                                   : std_logic_vector(31 downto 0) := (others => '0');
    signal curr_init_bram, next_init_bram               : std_logic_vector(1 downto 0)  := (others => '0');
    signal curr_counter, next_counter                   : std_logic_vector(1 downto 0);
    
    
    -- BOUNDARY CONDITIONS FOR UPPER AND LOWER ROWS(BRAM)
    signal curr_write_bound_buff, next_write_bound_buff : word_t;
    signal bound_en_write                               : std_logic;
    signal bound_en_read                                : std_logic;
    signal bound_we_write                               : std_logic;
    signal curr_bound_addr_ptr, next_bound_addr_ptr     : std_logic_vector(7 downto 0);
    signal bound_addr_write                             : std_logic_vector(7 downto 0);
    signal bound_addr_read                              : std_logic_vector(7 downto 0);
    signal bound_write                                  : std_logic_vector(31 downto 0);
    signal bound_read                                   : std_logic_vector(31 downto 0);
    
begin   

    ram_inst: three_dual_one_clock
        port map (
            clk                 => clk,
            ena                 => bram_en_write,
            enb                 => bram_en_read,
            wea                 => bram_we_write,
            addra               => bram_addr_write,
            addrb               => bram_addr_read,
            tag_offset          => tag_offset,
            dia                 => bram_write,
            dob1                => bram_read_row1,
            dob2                => bram_read_row2,
            dob3                => bram_read_row3
        );
        
    bound_ram_inst: simple_smaller
         port map(
              clk               => clk,
              ena               => bound_en_write,
              enb               => bound_en_read,
              wea               => bound_we_write,
              addra             => bound_addr_write,
              addrb             => bound_addr_read,
              dia               => bound_write,
              dob               => bound_read
         );
         
     boundary_conditions_inst: boundary_conditions
          port map( 
              row_1             => row_1(31 downto 0),
              row_2             => row_2(31 downto 0),
              row_3             => row_3(31 downto 0),
              en_flag           => en_flag,
              result            => result,
              result2           => result2,
              result3           => result3,
              result4           => result4
          );
        
    -- OUTPUT ASSIGNMENTS
    en <= curr_en;
    we <= curr_we; 
    addr <= curr_addr;

    -- COMBINATIONAL LOGIC TO TO DETERMINE THE CURRENT ADDRESS TO READ/WRITE
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


    -- COMBINATIONAL LOGIC FOR STATE TRANSITIONS
    cl_states: process(
        start, state,
        dataR, addr_write, addr_read, bound_read,  
        result, result2, result3, result4, -- The result computations  
        curr_addr_ptr, curr_write_buff, curr_write_bound_buff, curr_bram_addr_ptr, curr_bound_addr_ptr, 
        row_1, row_2, row_3, -- The 3 row shift registers
        curr_counter, bram_read_row1, bram_read_row2 -- The counter and values read from bram
    )
        constant row_2_offs  : integer := 88; -- The row offsets
        constant row_3_offs  : integer := 176;
        constant write_offs  : integer := 25344;
    begin
        -- Register updates
        next_state              <= state; 
        next_write_buff         <= curr_write_buff;
        next_write_bound_buff   <= curr_write_bound_buff;
        next_row_1              <= row_1;
        next_row_2              <= row_2;
        next_row_3              <= row_3; 
        ------------------------------ BRAM -------------------------------
        next_counter            <= curr_counter;
        next_addr_ptr           <= curr_addr_ptr;
        next_bram_addr_ptr      <= curr_bram_addr_ptr;
        next_bound_addr_ptr     <= curr_bound_addr_ptr;
        -------------------------------------------------------------------
        
        
        -- Initialization to hinder inferred latches
        finish                  <= '0';
        dataW                   <= (others => '0');
        curr_en                 <= '0';
        curr_we                 <= '0';
        addr_read               <= (others => '0');
        addr_write              <= (others => '0');
        ------------------------------ BRAM -------------------------------
        bram_we_write           <= '0';
        bram_en_write           <= '0';
        bram_en_read            <= '0';
        en_flag                 <= "110"; -- Row switching when reading from bram
        bram_addr_write         <= (others => 'Z');
        bram_addr_read          <= (others => 'Z');
        bram_write              <= (others => '0');
        tag_offset              <= (others => '0');
        
        bound_addr_read         <= (others => '0');
        bound_addr_write        <= (others => '0');    
        bound_write             <= (others => '0');    
        bound_en_write          <= '0'; -- For the upper and lower boundaries
        bound_we_write          <= '0';
        bound_en_read           <= '0';
        -------------------------------------------------------------------
        
  
        case state is
            -- S0: Idle
            when S0 =>
                curr_en             <= '1'; -- Read
                curr_we             <= '0';
                bram_we_write       <= '0'; 
                bram_en_write       <= '0';
                next_write_buff     <= (others => '0');
                next_addr_ptr       <= (others => '0');
                addr_read           <= (others => '0');
                addr_write          <= (others => '0');
                if start = '1' then
                    ------------------------------ BRAM -------------------------------
                    next_counter        <= (others => '0'); -- Instantiate the counter for tag offsets
                    bram_addr_write     <= (others => '0'); -- Instantiate reading and writing address
                    bram_addr_read      <= (others => '0'); -- Also setting up address location for the read in next state (1st row)
                    next_bram_addr_ptr  <= (others => '0'); -- Used for reading and writing at the correct offset
                    next_bound_addr_ptr <= (others => '0');
                    -------------------------------------------------------------------
                    next_state      <= I1;
                end if;
                
            -- Initialization states (I1 to I7): For setting up the shift registers
            when I1 =>
                next_row_1(31 downto 0)     <= dataR; -- Reading the data from the 1st row address location
                curr_en                     <= '1'; 
                curr_we                     <= '0';
                bram_we_write               <= '1'; 
                bram_en_write               <= '1'; 
                addr_read                   <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs); -- Setting up address location for the read in next state (2nd row) 
                ------------------------------ BRAM -------------------------------
                bram_addr_write             <= curr_bram_addr_ptr; -- To ensure dataR is stored in row1 (assuming we initialize at "00")
                bram_write                  <= dataR;
                tag_offset                  <= "10"; 
                -------------------------------------------------------------------
                next_state          <= I2;
            
            when I2 =>
                next_row_2(31 downto 0)     <= dataR; -- Reading the data from the 2nd row address location
                curr_en                     <= '1'; 
                curr_we                     <= '0';
                bram_we_write               <= '1'; 
                bram_en_write               <= '1';
                addr_read                   <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs); -- Setting up address location for the read in next state (3rd row)
                next_addr_ptr               <= std_logic_vector(unsigned(curr_addr_ptr) + 1); -- Incrementing the address pointer 
                ------------------------------ BRAM -------------------------------
                bram_addr_write             <= curr_bram_addr_ptr; -- To ensure dataR is stored in row2 (assuming we initialize at "00")
                bram_write                  <= dataR;
                tag_offset                  <= "01";  
                -------------------------------------------------------------------
                next_state                  <= I3;
                

            when I3 =>
                -- Reading the data from the 3rd row address location
                next_row_3(31 downto 0)     <= dataR;
                curr_en                     <= '1'; 
                curr_we                     <= '0';
                bram_we_write               <= '1'; 
                bram_en_write               <= '1';
                addr_read                   <= curr_addr_ptr; -- Updating the read address location after iterating the address pointer to the next word (i.e. next 4 pixels). Setting up address location for the read in next state (1st row)
                ------------------------------ BRAM -------------------------------
                next_bram_addr_ptr          <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1); -- Incrementing the BRAM pointer to write the next word of pixels (i.e. next 4 pixels) at an empty adjacent word location
                bram_addr_write             <= curr_bram_addr_ptr;
                bram_write                  <= dataR;
                tag_offset                  <= "00"; -- To ensure dataR is stored in row3 (assuming we initialize at "00")
                -------------------------------------------------------------------
                next_state      <= I4;

            when I4 =>
                next_row_1(63 downto 32)    <= dataR; -- Reading the data from the 1st row address location and have it buffered
                curr_en                     <= '1';
                curr_we                     <= '0';
                bram_we_write               <= '1';
                bram_en_write               <= '1';
                en_flag                     <= "000";
                addr_read  <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs); -- Setting up address location for the read in next state (2nd row)
                ------------------------------ BRAM -------------------------------
                bram_addr_write <= curr_bram_addr_ptr; -- Updating the BRAM write address location after iterating the BRAM address pointer to point to the next adjacent word (i.e. next 4 pixels) location
                bram_write <= dataR; 
                tag_offset <= "10"; -- To ensure buffered dataR is stored in row1 (assuming we initialize at "00")
                -------------------------------------------------------------------
                next_state      <= I5;

            when I5 =>
                next_row_2(63 downto 32)    <= dataR; -- Reading the data from the 2nd row address location and have it buffered
                curr_en                     <= '1';
                curr_we                     <= '0';
                bram_we_write               <= '1';
                bram_en_write               <= '1';
                addr_read                   <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs); -- Setting up address location for the read in next state (3rd row)
                next_addr_ptr               <= std_logic_vector(unsigned(curr_addr_ptr) + 1); -- Incrementing the address pointer 
                en_flag                     <= "000";
                next_write_buff(7 downto 0)         <= result;
                ------------------------------ BRAM -------------------------------
                bram_addr_write             <= curr_bram_addr_ptr; -- The left side
                next_write_bound_buff(7 downto 0)   <= result2; -- The upper side utilizes result2 in this phase
                bram_write                  <= dataR;  -- To ensure buffered dataR is stored in row2 (assuming we initialize at "00")
                tag_offset                  <= "01";
                -------------------------------------------------------------------
                next_state      <= I6;

            when I6 =>
                next_row_3(63 downto 32)    <= dataR; -- Reading the data from the 3rd row address location and have it buffered
                curr_en                     <= '1';
                curr_we                     <= '0';
                bram_we_write               <= '1';
                bram_en_write               <= '1';
                ------------------------------ BRAM -------------------------------
                bram_addr_write             <= curr_bram_addr_ptr; 
                next_bram_addr_ptr          <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1); -- Incrementing the BRAM pointer to write the next word of pixels (i.e. next 4 pixels) at an empty adjacent word location
                bram_write                  <= dataR; -- To ensure buffered dataR is stored in row3 (assuming we initialize at "00")
                tag_offset                  <= "00"; -- Due to init.: third row. 
                -------------------------------------------------------------------
                next_state      <= S1_init;
                

            -- Processing states (S1 to S4), assembling write buffer
            when S1_init =>
                curr_en                     <= '1';
                curr_we                     <= '0';
                bram_we_write               <= '0';
                bram_en_write               <= '0';
                addr_read                   <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs); -- Setting up address location for the read in next state (3rd row)
                next_row_1                  <= std_logic_vector(unsigned(row_1) srl 8); -- Shift rows by 1 pixel
                next_row_2                  <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3                  <= std_logic_vector(unsigned(row_3) srl 8);
                en_flag                     <= "111";
                next_write_buff(15 downto 8)        <= result; -- Loading the result for the 2nd pixel of result-buffer. This is due to the first pixel possibly being boundary
                ------------------------------ BRAM -------------------------------
                next_write_bound_buff(15 downto 8)  <= result2; -- The upper side 
                bram_addr_write             <= curr_bram_addr_ptr; -- Updating the BRAM write address location after iterating the BRAM address pointer to point to the next adjacent word (i.e. next 4 pixels) location
                -------------------------------------------------------------------
                next_state <= S2_init;      

            when S2_init =>
                curr_en                     <= '1';
                curr_we                     <= '0';
                bram_we_write               <= '1';
                bram_en_write               <= '1';
                addr_read                   <= std_logic_vector(unsigned(curr_addr_ptr) + row_2_offs); -- Setting up address location for the read in next state (2nd row)
                next_row_1                  <= std_logic_vector(unsigned(row_1) srl 8); -- Shift rows by 1 pixel
                next_row_2                  <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3                  <= std_logic_vector(unsigned(row_3) srl 8);
                next_row_3(79 downto 48)    <= dataR; -- Reading the data from the 3rd row address location
                en_flag                     <= "111";
                next_write_buff(23 downto 16)       <= result; -- Loading the result for the 3rd pixel of result-buffer. This is due to the first pixel possibly being boundary
                ------------------------------ BRAM -------------------------------
                next_write_bound_buff(23 downto 16) <= result2;
                bram_addr_write             <= curr_bram_addr_ptr; -- To ensure buffered dataR is stored in row3 (assuming we initialize at "00")
                if unsigned(curr_addr_ptr) > 87 then -- Ensuring the bram_write is written the correct row
                    bram_write      <= dataR;
                    tag_offset      <= curr_counter;
                else
                    bram_write      <= dataR;
                    tag_offset      <= "00";
                end if;
                -------------------------------------------------------------------
                next_state <= S3_init;

            when S3_init =>
                curr_en                     <= '1';
                curr_we                     <= '0';
                if unsigned(curr_addr_ptr) > 87 then -- To ensure buffered dataR is stored in row2 (assuming we initialize at "00")
                    bram_we_write           <= '0';
                    bram_en_write           <= '0';
                else
                    bram_we_write           <= '1';
                    bram_en_write           <= '1';
                    bram_write              <= dataR;
                    tag_offset              <= "01";
                end if;
                addr_read                   <= curr_addr_ptr; -- Setting up address location for the read in next state (1st row)
                next_row_1                  <= std_logic_vector(unsigned(row_1) srl 8); -- Shift rows by 1 pixel
                next_row_2                  <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3                  <= std_logic_vector(unsigned(row_3) srl 8);
                next_row_2(71 downto 40)    <= dataR; -- Reading the data from the 2rd row address location
                if  unsigned(curr_bram_addr_ptr) = 1 then -- Checks if the address pointer to the current calculations was at the boundaries 
                    en_flag                 <= "001";
                    else
                    en_flag                 <= "111";
                end if;
                next_write_buff(31 downto 24)       <= result;
                ------------------------------ BRAM -------------------------------
                next_write_bound_buff(31 downto 24) <= result2; -- The upper side
                bram_addr_write                     <= curr_bram_addr_ptr;
                -------------------------------------------------------------------
                next_state <= S4_init;
                
            when S4_init =>
                curr_en             <= '1';
                curr_we             <= '1';
                bound_en_write      <= '1';
                bound_we_write      <= '1';
                addr_read           <= curr_addr_ptr; -- Setting up address location for the read in next state
                bram_addr_write     <= curr_bram_addr_ptr;
                next_write_buff(7 downto 0)         <= result;
                next_addr_ptr               <= std_logic_vector(unsigned(curr_addr_ptr) + 1); -- Incrementing the address pointer 
                next_row_1                  <= std_logic_vector(unsigned(row_1) srl 8); -- Shift rows by 1 pixel
                next_row_2                  <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3                  <= std_logic_vector(unsigned(row_3) srl 8);
                next_row_1(63 downto 32)    <= dataR; -- Reading the data from the 3rd row address location
                addr_write                  <= std_logic_vector(unsigned(curr_addr_ptr) + write_offs - 2 + row_2_offs); -- Setting up address location for the write happening in the state transition
                dataW                       <= curr_write_buff; 
                next_write_bound_buff(7 downto 0)   <= result2;
                next_bound_addr_ptr         <= std_logic_vector(unsigned(curr_bound_addr_ptr) + 1); -- Increment buffer
                bound_addr_write            <= curr_bound_addr_ptr;
                bound_write                 <=  curr_write_bound_buff;
                ------------------------------ BRAM -------------------------------
                if unsigned(curr_addr_ptr) = 87 then -- Check if the write values are circling onto a new line
                    bram_en_write           <= '1';
                    bram_we_write           <= '1'; 
                    bram_en_read            <= '1';
                    next_bram_addr_ptr      <= (others => '0'); -- Reset the address pointer for the writing and reading
                    bram_write              <= dataR; -- To ensure buffered dataR is stored in row1 (assuming we initialize at "00")
                    tag_offset              <= "10";
                    next_counter            <= std_logic_vector(unsigned(curr_counter) + 2); -- Begin a cyclic counter (starts at "10")
                else
                    if unsigned(curr_addr_ptr) /= 89  then
                        next_bram_addr_ptr      <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1); -- Incrementing the BRAM pointer to write the next word of pixels (i.e. next 4 pixels) at an empty adjacent word location
                    end if;
                    if unsigned(curr_addr_ptr) > 87 then
                        bram_en_write       <= '0';
                        bram_we_write       <= '0'; 
                    else
                        bram_en_write       <= '1';
                        bram_we_write       <= '1';
                        bram_write          <= dataR; -- To ensure buffered dataR is stored in row1 (assuming we initialize at "00")
                        tag_offset          <= "10";
                    end if;
                end if;
                -------------------------------------------------------------------
                if unsigned(curr_bram_addr_ptr) = 1 then -- Check for the boundary conditions
                    en_flag                 <= "000";
                    next_state              <= S1_pre;
                else
                    en_flag                 <= "111";
                    next_state              <= S1_init;
                end if;
                
                
                
            when S1_pre =>
                en_flag                     <= "111";
                next_write_buff(15 downto 8)    <= result; -- Loading the result for the 2nd pixel of result-buffer. This is due to the first pixel possibly being boundary
                curr_en                     <= '1';
                curr_we                     <= '0';
                bram_we_write               <= '1'; 
                bram_en_write               <= '0';
                bram_en_read                <= '1'; 
                addr_read                   <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);  -- Setting up address location for the read in next state (3rd row). This will be the only row read from at this point forward
                next_row_1                  <= std_logic_vector(unsigned(row_1) srl 8); -- Shift rows by 1 pixel
                next_row_2                  <= std_logic_vector(unsigned(row_2) srl 8);
                next_row_3                  <= std_logic_vector(unsigned(row_3) srl 8);
                ------------------------------ BRAM -------------------------------
                bram_addr_read              <= curr_bram_addr_ptr; -- Updating the BRAM write address location after iterating the BRAM address pointer to point to the next adjacent word (i.e. next 4 pixels) location
                bram_addr_write             <= curr_bram_addr_ptr;
                -------------------------------------------------------------------
                next_state                  <= S1;    
            
            
            
            
            
            -------------------------------------------- SPEEDUP ---------------------------------------------------
            -- Now two rows wil always be buffered, which means only one row will be read from
            -- The shifting window is now buffered in memory, which allows 2 pixels to be calculated per. clock cycle
            -- S1 read pixels from main memory (2 computations)
            -- S2 write pixels into main memory (2 computations)
            --------------------------------------------------------------------------------------------------------
            when S1 => -- Check for end of processing excluding the last upper and lower boundaries
                if (unsigned(curr_addr_ptr) + row_3_offs = write_offs + 2) then
                    bound_en_read               <= '1';
                    bound_addr_read             <= (others  => '0');
                    next_bound_addr_ptr         <= std_logic_vector(to_unsigned(1, next_bound_addr_ptr'length));
                    next_addr_ptr               <= std_logic_vector(to_unsigned(1, next_addr_ptr'length));
                    next_state                  <= S_bound; -- Change
                else
                    curr_en        <= '1';
                    curr_we        <= '0';
                    bram_en_read   <= '1';
                    bram_we_write  <= '0';
                    bram_en_write  <= '1';
                    if unsigned(curr_bram_addr_ptr) = 87 then -- If the BRAM pointer reaches 87, i.e. end of line
                        next_bram_addr_ptr      <= (others => '0');
                        if unsigned(curr_counter) = 0 then
                            next_counter            <= std_logic_vector(unsigned(curr_counter) + 2);  -- Reset to 0
                        else
                            next_counter            <= std_logic_vector(unsigned(curr_counter) - 1);  -- Decrement
                        end if;
                    else
                        next_bram_addr_ptr <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1);
                    end if;
                    
                    -- Check for the boundary conditions
                    if unsigned(curr_bram_addr_ptr) = 0 then -- Loading the result for the 3rd and 4th pixel of result-buffer
                        en_flag                       <= "011";
                    else
                        en_flag                       <= "110";                  
                    end if;
                    next_write_buff(31 downto 16) <= result2 & result;
                    -- This will be the only row read from at this point forward
                    addr_read <= std_logic_vector(unsigned(curr_addr_ptr) + row_3_offs);
                    -- Shift rows by 2 pixels
                    next_row_1 <= std_logic_vector(unsigned(row_1) srl 16);
                    next_row_2 <= std_logic_vector(unsigned(row_2) srl 16);
                    next_row_3 <= std_logic_vector(unsigned(row_3) srl 16);
                    
                    ------------------------------ BRAM -------------------------------
                    -- Will start at the first row and iterate to the next rows
                    tag_offset <= curr_counter;
                    if unsigned(curr_bram_addr_ptr) = 87 then
                        bram_addr_read <= (others => '0');
                    else
                        bram_addr_read <= std_logic_vector(unsigned(curr_bram_addr_ptr) + 1);
                    end if;
                    if unsigned(curr_addr_ptr) > 25081 then -- Begin write the buffer for the last line 
                        next_write_bound_buff(31 downto 16) <= result4 & result3;        
                    end if;
                    -------------------------------------------------------------------  
                    next_state <= S2;
                end if;
                
            
            when S2 =>
                curr_en             <= '1';
                curr_we             <= '1';
                if unsigned(curr_addr_ptr) >= 25168 then
                    bram_we_write       <= '0';
                else 
                    bram_we_write       <= '1';
                end if;
                bram_en_write       <= '1';
                bram_en_read        <= '1';
                if unsigned(curr_bram_addr_ptr) = 1 then -- Check when we reach a new line
                    en_flag                         <= "010";
                else
                    en_flag                         <= "110";
                end if;
                tag_offset                  <= curr_counter; -- Will start at the first row and iterate to the next rows
                next_row_1                  <= std_logic_vector(unsigned(row_1) srl 16); -- Shift rows by 2 pixels
                next_row_2                  <= std_logic_vector(unsigned(row_2) srl 16);
                next_row_3                  <= std_logic_vector(unsigned(row_3) srl 16);
                next_row_1(55 downto 24)    <= bram_read_row1;
                next_row_2(55 downto 24)    <= bram_read_row2;
                next_row_3(55 downto 24)    <= dataR;
                addr_write                  <= std_logic_vector(unsigned(curr_addr_ptr) + write_offs - 2 + row_2_offs); -- Setting up address location for the write in happening in the state transition
                next_addr_ptr               <= std_logic_vector(unsigned(curr_addr_ptr) + 1); -- Incrementing the address pointer 
                next_write_buff(15 downto 0)            <=  result2 & result;
                ------------------------------ BRAM -------------------------------
                bram_addr_write                 <= curr_bram_addr_ptr; -- Updating the BRAM write address location after iterating the BRAM address pointer to point to the next adjacent word (i.e. next 4 pixels) location
                bram_write                      <= dataR; -- To ensure buffered dataR is stored in row2 (assuming we initialize at "00")
                tag_offset                      <= curr_counter;
                -- Evaluate conditions separately to reduce logic depth
                if unsigned(curr_addr_ptr) > 25081 then
                    bound_en_write          <= '1';
                    bound_we_write          <= '1'; 
                    next_bound_addr_ptr     <= std_logic_vector(unsigned(curr_bound_addr_ptr) + 1); -- Increment buffer
                end if;
                if unsigned(curr_addr_ptr) > 25080 then
                    next_write_bound_buff(15 downto 0)  <= result4 & result3;
                    bound_addr_write        <= curr_bound_addr_ptr;
                    bound_write             <= curr_write_bound_buff;
                end if;

                -------------------------------------------------------------------
                dataW                       <= curr_write_buff;
                next_state                  <= S1;
            
            
            when S_bound =>
                curr_en        <= '1';
                curr_we        <= '1';
                bound_en_read  <= '1';
                if unsigned(curr_addr_ptr) < 89 then
                    addr_write                  <= std_logic_vector(write_offs + unsigned(curr_addr_ptr) - 1);
                else
                    addr_write                  <= std_logic_vector(write_offs + 25168 + unsigned(curr_addr_ptr) - 1); -- Offset to the lowest row
                end if;
                bound_addr_read             <= std_logic_vector(curr_bound_addr_ptr);
                next_bound_addr_ptr             <= std_logic_vector(unsigned(curr_bound_addr_ptr) + 1);
                next_addr_ptr                   <= std_logic_vector(unsigned(curr_addr_ptr) + 1);
                dataW                           <= bound_read;
                if unsigned(curr_addr_ptr) = 176 then
                    next_state <= F;
                else
                    next_state <= S_bound;
                end if;
                
            
            -- Finish state
            when F =>
                finish              <= '1';
                if start = '1' then
                    next_state          <= F;
                else
                    next_state          <= S0;
                end if;
            
            -- Default case
            when others =>
                next_state              <= S0;
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
                curr_write_bound_buff <= (others => '0');
                row_1           <= (others => '0');
                row_2           <= (others => '0');
                row_3           <= (others => '0');
                -- Block mem --
                curr_bram_addr_ptr  <= (others => '0');
                curr_counter        <= (others => '0');
                curr_bound_addr_ptr <= (others => '0');
                -- --------- --
            else
                -- Update current state and signals
                state           <= next_state;
                curr_addr_ptr   <= next_addr_ptr;
                row_1           <= next_row_1;
                row_2           <= next_row_2;
                row_3           <= next_row_3;
                curr_write_buff <= next_write_buff;
                curr_write_bound_buff <= next_write_bound_buff;
                -- Block mem --
                curr_counter <= next_counter;
                curr_bram_addr_ptr <= next_bram_addr_ptr;
                curr_bound_addr_ptr <= next_bound_addr_ptr;
                -- --------- --
            end if;
        end if;
    end process myprocess;

end rtl;