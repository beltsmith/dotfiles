{ pkgs, ... }:

{
  packages = with pkgs; [ ffmpeg handbrake ];
}
