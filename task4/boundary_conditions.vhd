----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/26/2024 06:33:42 PM
-- Design Name: 
-- Module Name: boundary_conditions - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.04 - Extended calculations to four Sobel sums
-- Additional Comments:
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.types.all;

entity boundary_conditions is
    port ( 
        row_1          : in std_logic_vector(31 downto 0); 
        row_2          : in std_logic_vector(31 downto 0);
        row_3          : in std_logic_vector(31 downto 0);
        addr_ptr       : in std_logic_vector(15 downto 0);
        bram_addr_ptr  : in std_logic_vector(6 downto 0);
        en_flag        : in std_logic_vector(2 downto 0); -- Informs if it is the first two pixels (0) or the last 2 pixels (1)
        result         : out std_logic_vector(7 downto 0);
        result2        : out std_logic_vector(7 downto 0);
        result3        : out std_logic_vector(7 downto 0);
        result4        : out std_logic_vector(7 downto 0)
    );
end boundary_conditions;

architecture Behavioral of boundary_conditions is
    
    begin
    
    -- Combinational process for Dx, Dy, and Sobel filter computation
    cl_sobel: process(row_1, row_2, row_3, addr_ptr, bram_addr_ptr, en_flag)
        -- For the first Sobel sum
        variable s11_v, s12_v, s13_v : std_logic_vector(7 downto 0);
        variable s21_v, s22_v, s23_v : std_logic_vector(7 downto 0);
        variable s31_v, s32_v, s33_v : std_logic_vector(7 downto 0);
    
        -- For the second Sobel sum
        variable s11_2v, s12_2v, s13_2v : std_logic_vector(7 downto 0);
        variable s21_2v, s22_2v, s23_2v : std_logic_vector(7 downto 0);
        variable s31_2v, s32_2v, s33_2v : std_logic_vector(7 downto 0);
    
        -- For the third Sobel sum
        variable s11_3v, s12_3v, s13_3v : std_logic_vector(7 downto 0);
        variable s21_3v, s22_3v, s23_3v : std_logic_vector(7 downto 0);
        variable s31_3v, s32_3v, s33_3v : std_logic_vector(7 downto 0);
    
        -- For the fourth Sobel sum
        variable s11_4v, s12_4v, s13_4v : std_logic_vector(7 downto 0);
        variable s21_4v, s22_4v, s23_4v : std_logic_vector(7 downto 0);
        variable s31_4v, s32_4v, s33_4v : std_logic_vector(7 downto 0);
    
        -- Variables for gradient calculations
        variable dx_v, dy_v          : signed(11 downto 0);   -- 12-bit signed for intermediate calculations
        variable abs_dx_v, abs_dy_v  : unsigned(11 downto 0); -- 12-bit unsigned for absolute values
        variable sobel_sum_v         : unsigned(11 downto 0); -- 12-bit unsigned for sum of gradients
        variable result_v            : std_logic_vector(7 downto 0);  -- 8-bit output
    
        variable dx2_v, dy2_v        : signed(11 downto 0);
        variable abs_dx2_v, abs_dy2_v: unsigned(11 downto 0);
        variable sobel_sum2_v        : unsigned(11 downto 0);
        variable result2_v           : std_logic_vector(7 downto 0);
    
        variable dx3_v, dy3_v        : signed(11 downto 0);
        variable abs_dx3_v, abs_dy3_v: unsigned(11 downto 0);
        variable sobel_sum3_v        : unsigned(11 downto 0);
        variable result3_v           : std_logic_vector(7 downto 0);
    
        variable dx4_v, dy4_v        : signed(11 downto 0);
        variable abs_dx4_v, abs_dy4_v: unsigned(11 downto 0);
        variable sobel_sum4_v        : unsigned(11 downto 0);
        variable result4_v           : std_logic_vector(7 downto 0);
    
    begin
    
        -- Default assignments to prevent latches
        s11_v := (others => '0');
        s12_v := (others => '0');
        s13_v := (others => '0');
        s21_v := (others => '0');
        s22_v := (others => '0');
        s23_v := (others => '0');
        s31_v := (others => '0');
        s32_v := (others => '0');
        s33_v := (others => '0');
    
        s11_2v := (others => '0');
        s12_2v := (others => '0');
        s13_2v := (others => '0');
        s21_2v := (others => '0');
        s22_2v := (others => '0');
        s23_2v := (others => '0');
        s31_2v := (others => '0');
        s32_2v := (others => '0');
        s33_2v := (others => '0');
    
        s11_3v := (others => '0');
        s12_3v := (others => '0');
        s13_3v := (others => '0');
        s21_3v := (others => '0');
        s22_3v := (others => '0');
        s23_3v := (others => '0');
        s31_3v := (others => '0');
        s32_3v := (others => '0');
        s33_3v := (others => '0');
    
        s11_4v := (others => '0');
        s12_4v := (others => '0');
        s13_4v := (others => '0');
        s21_4v := (others => '0');
        s22_4v := (others => '0');
        s23_4v := (others => '0');
        s31_4v := (others => '0');
        s32_4v := (others => '0');
        s33_4v := (others => '0');
    
        -- Assign pixel values based on the case statements
        case bram_addr_ptr is
            when "0000001" =>
                case en_flag is
                    when "000" => -- Left corner for single pixel [INIT PHASE]
                        s11_v := row_1(7 downto 0); -- 2nd row left most pixel
                        s12_v := row_1(7 downto 0); 
                        s13_v := row_1(15 downto 8);
                        s21_v := row_2(7 downto 0);
                        s22_v := row_2(7 downto 0);
                        s23_v := row_2(15 downto 8);
                        s31_v := row_3(7 downto 0);
                        s32_v := row_3(7 downto 0);
                        s33_v := row_3(15 downto 8);
                        ---------------------- UPPER / LOWER BOUNDARY CONDITIONS ------------------------
                        s11_2v := row_1(7 downto 0); -- Upper corner left most pixel
                        s12_2v := row_1(7 downto 0);
                        s13_2v := row_1(15 downto 8);
                        s21_2v := row_1(7 downto 0);
                        s22_2v := row_1(7 downto 0);
                        s23_2v := row_1(15 downto 8);
                        s31_2v := row_2(7 downto 0);
                        s32_2v := row_2(7 downto 0);
                        s33_2v := row_2(15 downto 8);
                        ---------------------------------------------------------------------------------
                        
                    when "001" => -- Right corner for single pixel [INIT PHASE]
                        s11_v := row_1(15 downto 8); -- 2nd row right most pixel
                        s12_v := row_1(23 downto 16); 
                        s13_v := row_1(23 downto 16);
                        s21_v := row_2(15 downto 8);
                        s22_v := row_2(23 downto 16);
                        s23_v := row_2(23 downto 16);
                        s31_v := row_3(15 downto 8);
                        s32_v := row_3(23 downto 16);
                        s33_v := row_3(23 downto 16);
                        ---------------------- UPPER / LOWER BOUNDARY CONDITIONS ------------------------
                        s11_2v := row_1(15 downto 8); -- Upper corner right most pixel
                        s12_2v := row_1(23 downto 16);
                        s13_2v := row_1(23 downto 16);
                        s21_2v := row_1(15 downto 8);
                        s22_2v := row_1(23 downto 16);
                        s23_2v := row_1(23 downto 16);
                        s31_2v := row_2(15 downto 8);
                        s32_2v := row_2(23 downto 16);
                        s33_2v := row_2(23 downto 16);
                        ---------------------------------------------------------------------------------
                        
                    when "010" => -- Left side for values for 2 concatenated results at a time  [SPEEDUP PHASE]
                        s11_v := row_1(7 downto 0);
                        s12_v := row_1(7 downto 0);
                        s13_v := row_1(15 downto 8);
                        s21_v := row_2(7 downto 0);
                        s22_v := row_2(7 downto 0);
                        s23_v := row_2(15 downto 8);
                        s31_v := row_3(7 downto 0);
                        s32_v := row_3(7 downto 0);
                        s33_v := row_3(15 downto 8);
                        
                        s11_2v := row_1(15 downto 8);
                        s12_2v := row_1(23 downto 16);
                        s13_2v := row_1(31 downto 24);
                        s21_2v := row_2(15 downto 8);
                        s22_2v := row_2(23 downto 16);
                        s23_2v := row_2(31 downto 24);
                        s31_2v := row_3(15 downto 8);
                        s32_2v := row_3(23 downto 16);
                        s33_2v := row_3(31 downto 24);
                        ---------------------- UPPER / LOWER BOUNDARY CONDITIONS ------------------------
                        s11_3v := row_2(7 downto 0); -- Left corner right most pixel
                        s12_3v := row_2(7 downto 0);
                        s13_3v := row_2(15 downto 8);
                        s21_3v := row_3(7 downto 0);
                        s22_3v := row_3(7 downto 0);
                        s23_3v := row_3(15 downto 8);
                        s31_3v := row_3(7 downto 0);
                        s32_3v := row_3(7 downto 0);
                        s33_3v := row_3(15 downto 8);
                    
                        s11_4v := row_2(15 downto 8);    
                        s12_4v := row_2(23 downto 16);  
                        s13_4v := row_2(31 downto 24);   
                        s21_4v := row_3(15 downto 8);    
                        s22_4v := row_3(23 downto 16);   
                        s23_4v := row_3(31 downto 24);   
                        s31_4v := row_3(15 downto 8);    
                        s32_4v := row_3(23 downto 16);   
                        s33_4v := row_3(31 downto 24);   
                        ---------------------------------------------------------------------------------
                        
                    when "011" => -- Right side for values for 2 concatenated results at a time  [SPEEDUP PHASE]
                        s11_v := row_1(7 downto 0);
                        s12_v := row_1(15 downto 8);
                        s13_v := row_1(23 downto 16);
                        s21_v := row_2(7 downto 0);
                        s22_v := row_2(15 downto 8);
                        s23_v := row_2(23 downto 16);
                        s31_v := row_3(7 downto 0);
                        s32_v := row_3(15 downto 8);
                        s33_v := row_3(23 downto 16);
    
                        s11_2v := row_1(15 downto 8);
                        s12_2v := row_1(23 downto 16);
                        s13_2v := row_1(23 downto 16);
                        s21_2v := row_2(15 downto 8);
                        s22_2v := row_2(23 downto 16);
                        s23_2v := row_2(23 downto 16);
                        s31_2v := row_3(15 downto 8);
                        s32_2v := row_3(23 downto 16);
                        s33_2v := row_3(23 downto 16);
                        
                        ---------------------- UPPER / LOWER BOUNDARY CONDITIONS ------------------------
                        s11_3v := row_2(7 downto 0); -- Right corner right most pixel 
                        s12_3v := row_2(15 downto 8);
                        s13_3v := row_2(23 downto 16);
                        s21_3v := row_3(7 downto 0);
                        s22_3v := row_3(15 downto 8);
                        s23_3v := row_3(23 downto 16);
                        s31_3v := row_3(7 downto 0);
                        s32_3v := row_3(15 downto 8);
                        s33_3v := row_3(23 downto 16);
                    
                        s11_4v := row_2(23 downto 16);    
                        s12_4v := row_2(31 downto 24);    
                        s13_4v := row_2(31 downto 24);    
                        s21_4v := row_3(23 downto 16);    
                        s22_4v := row_3(31 downto 24);    
                        s23_4v := row_3(31 downto 24);    
                        s31_4v := row_3(23 downto 16);    
                        s32_4v := row_3(31 downto 24);    
                        s33_4v := row_3(31 downto 24);    
                        ---------------------------------------------------------------------------------
                        
                    when others =>
                        -- Default assignments for other cases
                        s11_v := row_1(7 downto 0);
                        s12_v := row_1(15 downto 8);
                        s13_v := row_1(23 downto 16);
                        s21_v := row_2(7 downto 0);
                        s22_v := row_2(15 downto 8);
                        s23_v := row_2(23 downto 16);
                        s31_v := row_3(7 downto 0);
                        s32_v := row_3(15 downto 8);
                        s33_v := row_3(23 downto 16);
                        
                        s11_2v := row_1(15 downto 8);
                        s12_2v := row_1(23 downto 16);
                        s13_2v := row_1(31 downto 24);
                        s21_2v := row_2(15 downto 8);
                        s22_2v := row_2(23 downto 16);
                        s23_2v := row_2(31 downto 24);
                        s31_2v := row_3(15 downto 8);
                        s32_2v := row_3(23 downto 16);
                        s33_2v := row_3(31 downto 24);
                        
                        s11_3v := row_2(7 downto 0); 
                        s12_3v := row_2(15 downto 8);
                        s13_3v := row_2(23 downto 16);
                        s21_3v := row_3(7 downto 0);
                        s22_3v := row_3(15 downto 8);
                        s23_3v := row_3(23 downto 16);
                        s31_3v := row_3(7 downto 0);
                        s32_3v := row_3(15 downto 8);
                        s33_3v := row_3(23 downto 16);
                        
                        s11_4v := row_2(15 downto 8);    
                        s12_4v := row_2(23 downto 16);  
                        s13_4v := row_2(31 downto 24);   
                        s21_4v := row_3(15 downto 8);    
                        s22_4v := row_3(23 downto 16);   
                        s23_4v := row_3(31 downto 24);   
                        s31_4v := row_3(15 downto 8);    
                        s32_4v := row_3(23 downto 16);   
                        s33_4v := row_3(31 downto 24);
                        
                end case;
              
            when others =>
            
                if unsigned(addr_ptr) < 90 then -- The upper side for single pixel [INIT PHASE]
                        s11_v := row_1(7 downto 0);
                        s12_v := row_1(15 downto 8);
                        s13_v := row_1(23 downto 16);
                        s21_v := row_2(7 downto 0);
                        s22_v := row_2(15 downto 8);
                        s23_v := row_2(23 downto 16);
                        s31_v := row_3(7 downto 0);
                        s32_v := row_3(15 downto 8);
                        s33_v := row_3(23 downto 16);
                        ---------------------- UPPER / LOWER BOUNDARY CONDITIONS ------------------------
                        s11_2v := row_1(7 downto 0); -- Upper side pixel
                        s12_2v := row_1(15 downto 8);
                        s13_2v := row_1(23 downto 16);
                        s21_2v := row_1(7 downto 0);
                        s22_2v := row_1(15 downto 8);
                        s23_2v := row_1(23 downto 16);
                        s31_2v := row_2(7 downto 0);
                        s32_2v := row_2(15 downto 8);
                        s33_2v := row_2(23 downto 16);
                        ---------------------------------------------------------------------------------
                else
                        -- Default assignments for other cases
                        s11_v := row_1(7 downto 0);
                        s12_v := row_1(15 downto 8);
                        s13_v := row_1(23 downto 16);
                        s21_v := row_2(7 downto 0);
                        s22_v := row_2(15 downto 8);
                        s23_v := row_2(23 downto 16);
                        s31_v := row_3(7 downto 0);
                        s32_v := row_3(15 downto 8);
                        s33_v := row_3(23 downto 16);
            
                        s11_2v := row_1(15 downto 8);
                        s12_2v := row_1(23 downto 16);
                        s13_2v := row_1(31 downto 24);
                        s21_2v := row_2(15 downto 8);
                        s22_2v := row_2(23 downto 16);
                        s23_2v := row_2(31 downto 24);
                        s31_2v := row_3(15 downto 8);
                        s32_2v := row_3(23 downto 16);
                        s33_2v := row_3(31 downto 24);
                        
                        s11_3v := row_2(7 downto 0); 
                        s12_3v := row_2(15 downto 8);
                        s13_3v := row_2(23 downto 16);
                        s21_3v := row_3(7 downto 0);
                        s22_3v := row_3(15 downto 8);
                        s23_3v := row_3(23 downto 16);
                        s31_3v := row_3(7 downto 0);
                        s32_3v := row_3(15 downto 8);
                        s33_3v := row_3(23 downto 16);
                        
                        s11_4v := row_2(15 downto 8);    
                        s12_4v := row_2(23 downto 16);  
                        s13_4v := row_2(31 downto 24);   
                        s21_4v := row_3(15 downto 8);    
                        s22_4v := row_3(23 downto 16);   
                        s23_4v := row_3(31 downto 24);   
                        s31_4v := row_3(15 downto 8);    
                        s32_4v := row_3(23 downto 16);   
                        s33_4v := row_3(31 downto 24);
                end if;
                
            
                
            end case;
            
    
        -- Compute gradients for the first result
        dx_v := signed("0000" & s13_v) - signed("0000" & s11_v)
                + (signed("0000" & s23_v) sll 1) - (signed("0000" & s21_v) sll 1)
                + signed("0000" & s33_v) - signed("0000" & s31_v);
    
        dy_v := signed("0000" & s11_v) - signed("0000" & s31_v)
                + (signed("0000" & s12_v) sll 1) - (signed("0000" & s32_v) sll 1)
                + signed("0000" & s13_v) - signed("0000" & s33_v);
    
        -- Compute absolute values and sum them
        abs_dx_v := unsigned(abs(dx_v));
        abs_dy_v := unsigned(abs(dy_v));
    
        -- Sum of absolute gradients
        sobel_sum_v := abs_dx_v + abs_dy_v;
    
        -- Scale the result to 8 bits
        result_v := std_logic_vector(sobel_sum_v(10 downto 3));
    
        -- Compute gradients for the second result
        dx2_v := signed("0000" & s13_2v) - signed("0000" & s11_2v)
                 + (signed("0000" & s23_2v) sll 1) - (signed("0000" & s21_2v) sll 1)
                 + signed("0000" & s33_2v) - signed("0000" & s31_2v);
    
        dy2_v := signed("0000" & s11_2v) - signed("0000" & s31_2v)
                 + (signed("0000" & s12_2v) sll 1) - (signed("0000" & s32_2v) sll 1)
                 + signed("0000" & s13_2v) - signed("0000" & s33_2v);
    
        -- Compute absolute values and sum them
        abs_dx2_v := unsigned(abs(dx2_v));
        abs_dy2_v := unsigned(abs(dy2_v));
    
        -- Sum of absolute gradients
        sobel_sum2_v := abs_dx2_v + abs_dy2_v;
    
        -- Scale the result2 to 8 bits
        result2_v := std_logic_vector(sobel_sum2_v(10 downto 3));
    
        -- Compute gradients for the third result
        dx3_v := signed("0000" & s13_3v) - signed("0000" & s11_3v)
                 + (signed("0000" & s23_3v) sll 1) - (signed("0000" & s21_3v) sll 1)
                 + signed("0000" & s33_3v) - signed("0000" & s31_3v);
    
        dy3_v := signed("0000" & s11_3v) - signed("0000" & s31_3v)
                 + (signed("0000" & s12_3v) sll 1) - (signed("0000" & s32_3v) sll 1)
                 + signed("0000" & s13_3v) - signed("0000" & s33_3v);
    
        -- Compute absolute values and sum them
        abs_dx3_v := unsigned(abs(dx3_v));
        abs_dy3_v := unsigned(abs(dy3_v));
    
        -- Sum of absolute gradients
        sobel_sum3_v := abs_dx3_v + abs_dy3_v;
    
        -- Scale the result3 to 8 bits
        result3_v := std_logic_vector(sobel_sum3_v(10 downto 3));
    
        -- Compute gradients for the fourth result
        dx4_v := signed("0000" & s13_4v) - signed("0000" & s11_4v)
                 + (signed("0000" & s23_4v) sll 1) - (signed("0000" & s21_4v) sll 1)
                 + signed("0000" & s33_4v) - signed("0000" & s31_4v);
    
        dy4_v := signed("0000" & s11_4v) - signed("0000" & s31_4v)
                 + (signed("0000" & s12_4v) sll 1) - (signed("0000" & s32_4v) sll 1)
                 + signed("0000" & s13_4v) - signed("0000" & s33_4v);
    
        -- Compute absolute values and sum them
        abs_dx4_v := unsigned(abs(dx4_v));
        abs_dy4_v := unsigned(abs(dy4_v));
    
        -- Sum of absolute gradients
        sobel_sum4_v := abs_dx4_v + abs_dy4_v;
    
        -- Scale the result4 to 8 bits
        result4_v := std_logic_vector(sobel_sum4_v(10 downto 3));
    
        -- Assign the computed results to output signals
        result  <= result_v;
        result2 <= result2_v;
        result3 <= result3_v;
        result4 <= result4_v;
        
    end process cl_sobel;
    
    end Behavioral;
