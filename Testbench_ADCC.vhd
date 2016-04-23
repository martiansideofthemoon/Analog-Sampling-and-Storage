library ieee;
use ieee.std_logic_1164.all;
library std;
use std.textio.all;
library work;
use work.ADCCComponents.all;

entity Testbench_ADCC is
end entity;
architecture Behave of Testbench_ADCC is
  signal din, adc_data: std_logic_vector(7 downto 0);
  signal CS, WR, RD, INTR, adc_output_ready, adc_run: std_logic;
  signal clk: std_logic := '0';
  signal reset: std_logic := '1';

  function to_string(x: string) return string is
      variable ret_val: string(1 to x'length);
      alias lx : string (1 to x'length) is x;
  begin
      ret_val := lx;
      return(ret_val);
  end to_string;

  function to_std_logic_vector(x: bit_vector) return std_logic_vector is
    alias lx: bit_vector(1 to x'length) is x;
    variable ret_var : std_logic_vector(1 to x'length);
  begin
     for I in 1 to x'length loop
        if(lx(I) = '1') then
           ret_var(I) :=  '1';
        else
           ret_var(I) :=  '0';
	end if;
     end loop;
     return(ret_var);
  end to_std_logic_vector;

begin
  clk <= not clk after 10 ns; -- assume 10ns clock.

  process
    begin
      wait until clk = '1';
      reset <= '0';
      wait;
  end process;

  process
    variable err_flag : boolean := false;
    File INFILE: text open read_mode is "TRACEFILE_ADCC.txt";
    FILE OUTFILE: text  open write_mode is "OUTPUTS_ADCC.txt";

    ---------------------------------------------------
    -- edit the next few lines to customize
    variable number: bit_vector (7 downto 0);
    ----------------------------------------------------
    variable INPUT_LINE: Line;
    variable OUTPUT_LINE: Line;
    variable LINE_COUNT: integer := 0;
    begin

    wait until clk = '1';

    while not endfile(INFILE) loop
    	  wait until clk = '0';

        LINE_COUNT := LINE_COUNT + 1;

	  readLine (INFILE, INPUT_LINE);
          read (INPUT_LINE, number);

          --------------------------------------
          -- from input-vector to DUT inputs
	  --dividend <= to_std_logic_vector(A_var);
	  --divisor <= to_std_logic_vector(B_var);
          --------------------------------------

      -- set start
      adc_run <= '1';
      INTR <= '1';

      -- wait until WR = '0'
      while (true) loop
        wait until clk = '1';
        if (WR = '0') then
          din <= to_std_logic_vector(number);
          INTR <= '0' after 215 us;
          exit;
        end if;
      end loop;

      -- wait until adc_out_ready is 1
      while (true) loop
        wait until clk = '1';
        if(adc_output_ready = '1') then
          exit;
        end if;
      end loop;
          --------------------------------------
	  -- check outputs.
	  if (adc_data /= to_std_logic_vector(number)) then
      write(OUTPUT_LINE,to_string("ERROR: in RESULT, line "));
      write(OUTPUT_LINE, LINE_COUNT);
      writeline(OUTFILE, OUTPUT_LINE);
      err_flag := true;
    end if;
          --------------------------------------
    end loop;

    assert (err_flag) report "SUCCESS, all tests passed." severity note;
    assert (not err_flag) report "FAILURE, some tests failed." severity error;

    wait;
  end process;

  dut: ADCC
    port map(
      din => din,
      adc_data => adc_data,
      CS => CS, WR => WR, RD => RD,
      INTR => INTR,
      adc_run => adc_run,
      adc_output_ready => adc_output_ready,
      clk => clk, reset => reset
    );

end Behave;

