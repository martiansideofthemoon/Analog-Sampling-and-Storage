library ieee;
use ieee.std_logic_1164.all;
library ieee;
use ieee.numeric_std.all;
library std;
use std.textio.all;

entity Testbench_SMC is
end entity;
architecture Behave of Testbench_SMC is
  component SMC is
    port(
      io: inout std_logic_vector(7 downto 0);
      mc_read_data: out std_logic_vector(7 downto 0);
      mc_write_data: in std_logic_vector(7 downto 0);
      mc_done: out std_logic;
      mc_start, mc_write: in std_logic;
      OE, WE, CS: out std_logic;
      mc_address: in std_logic_vector(12 downto 0);
      address: out std_logic_vector(12 downto 0);
      clk, reset: in std_logic
    );
  end component SMC;
  type mem_type is array(255 downto 0) of std_logic_vector(7 downto 0);
  signal io: std_logic_vector(7 downto 0);
  signal mc_read_data: std_logic_vector(7 downto 0);
  signal mc_write_data: std_logic_vector(7 downto 0);
  signal OE, WE, CS, mc_done, mc_start, mc_write: std_logic;
  signal mc_address, address: std_logic_vector(12 downto 0);
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

  function vec2int(vec1: bit_vector)
    return integer is
  variable retval: integer:=0;
  alias vec: bit_vector(vec1'length-1 downto 0) is vec1;
  begin
    for i in vec'high downto 1 loop
      if (vec(i)='1') then
        retval:=(retval+1)*2;
      else
        retval:=retval*2;
      end if;
    end loop;
    if vec(0)='1' then
      retval:=retval+1;
    end if;
    return retval;
  end vec2int;

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
    File INFILE: text open read_mode is "TRACEFILE_SMC.txt";
    FILE OUTFILE: text  open write_mode is "OUTPUTS_SMC.txt";

    ---------------------------------------------------
    -- edit the next few lines to customize
    variable mode: bit_vector (0 downto 0);
    variable address: bit_vector (12 downto 0);
    variable data: bit_vector (7 downto 0);
    variable ram: mem_type;
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
          read (INPUT_LINE, mode);
          read (INPUT_LINE, address);
          read (INPUT_LINE, data);
    if (to_std_logic_vector(mode) = "1") then
      ram(vec2int(address)) := to_std_logic_vector(data);
    end if;
   --       --------------------------------------
   --       -- from input-vector to DUT inputs
	  --dividend <= to_std_logic_vector(A_var);
	  --divisor <= to_std_logic_vector(B_var);
   --       --------------------------------------

      -- set start
      mc_address <= to_std_logic_vector(address);
      mc_start <= '1';
      if (mode = "1") then
        mc_write <= '1';
      else
        mc_write <= '0';
      end if;
      --mc_write <= to_std_logic_vector(mode)(0);

      -- check value of mc_write
      wait until clk = '1';
      if (mc_write = '1') then
        mc_write_data <= to_std_logic_vector(data);
        -- wait until WE is '0'
        while (true) loop
          wait until clk = '1';
          if (WE = '0') then
            io <= "ZZZZZZZZ";
            exit;
          end if;
        end loop;
        -- wait until mc_done is 1
        while (true) loop
          wait until clk = '1';
          if (mc_done = '1') then
            exit;
          end if;
        end loop;
      else
        -- wait until OE is '0'
        while (true) loop
        wait until clk = '1';
          if (OE = '0') then
            io <= ram(vec2int(address)) after 35 ns;
            exit;
          end if;
        end loop;
        -- wait until mc_done is 1
        while (true) loop
          wait until clk = '1';
          if (mc_done = '1') then
            exit;
          end if;
        end loop;
        -- wait until mc_done is 0, and set OE properly
        while (true) loop
          wait until clk = '1';
          if (OE = '1') then
            io <= "ZZZZZZZZ" after 35 ns;
          end if;
          if (mc_done = '0') then
            exit;
          end if;
        end loop;
      end if;
          --------------------------------------
	  -- check outputs.
	  if (mode = "0" and mc_read_data /= to_std_logic_vector(data)) then
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

  dut: SMC
    port map(
      io => io,
      mc_read_data => mc_read_data,
      mc_write_data => mc_write_data,
      mc_done => mc_done,
      mc_start => mc_start,
      mc_write => mc_write,
      OE => OE,
      WE => WE,
      CS => CS,
      mc_address => mc_address,
      address => address,
      clk => clk,
      reset => reset
    );

end Behave;

