import * as $filepath from "../../filepath/filepath.mjs";
import * as $crypto from "../../gleam_crypto/gleam/crypto.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import { Execute, FilePermissions, Read, Write } from "../../simplifile/simplifile.mjs";
import { Ok, Error, toList } from "../gleam.mjs";
import * as $cli from "../lustre_dev_tools/cli.mjs";
import * as $cmd from "../lustre_dev_tools/cmd.mjs";
import * as $error from "../lustre_dev_tools/error.mjs";
import {
  BundleError,
  CannotSetPermissions,
  CannotWriteFile,
  NetworkError,
  UnknownPlatform,
  UnzipError,
} from "../lustre_dev_tools/error.mjs";
import * as $project from "../lustre_dev_tools/project.mjs";
import * as $utils from "../lustre_dev_tools/utils.mjs";

function check_esbuild_exists(path) {
  let $ = $simplifile.is_file(path);
  if ($.isOk() && $[0]) {
    return true;
  } else if ($.isOk() && !$[0]) {
    return false;
  } else {
    return false;
  }
}

function get_download_url_and_hash(os, cpu) {
  let base = "https://registry.npmjs.org/@esbuild/";
  if (os === "android" && cpu === "arm") {
    return new Ok(
      [
        base + "android-arm/-/android-arm-0.25.2.tgz",
        "B8DBDA0352E2EF679507DE0010C18705CBC7FCD5402ED1BD105B2707FCA4CCA1",
      ],
    );
  } else if (os === "android" && cpu === "arm64") {
    return new Ok(
      [
        base + "android-arm64/-/android-arm64-0.25.2.tgz",
        "113FD6E8D1381C1EC329E0EAB3D95361211ABAA5231C16CE2BE89B54FA9EC1F2",
      ],
    );
  } else if (os === "android" && cpu === "x64") {
    return new Ok(
      [
        base + "android-x64/-/android-x64-0.25.2.tgz",
        "0966C109B386D137C9092405B199C715C72B5CE8B9A1FA3D8C6B9D4C780FB9D8",
      ],
    );
  } else if (os === "darwin" && cpu === "aarch64") {
    return new Ok(
      [
        base + "darwin-arm64/-/darwin-arm64-0.25.2.tgz",
        "D51E19BA63FE86A6FB6B70596AA0CA32362676274A50CFF6F4EF6E2DE02F4E4A",
      ],
    );
  } else if (os === "darwin" && cpu === "arm64") {
    return new Ok(
      [
        base + "darwin-arm64/-/darwin-arm64-0.25.2.tgz",
        "D51E19BA63FE86A6FB6B70596AA0CA32362676274A50CFF6F4EF6E2DE02F4E4A",
      ],
    );
  } else if (os === "darwin" && cpu === "amd64") {
    return new Ok(
      [
        base + "darwin-x64/-/darwin-x64-0.25.2.tgz",
        "DE2B564CB345FFAA6CD031A3C40920E8C3658A3321B864F53252F22D810380C5",
      ],
    );
  } else if (os === "darwin" && cpu === "x86_64") {
    return new Ok(
      [
        base + "darwin-x64/-/darwin-x64-0.25.2.tgz",
        "DE2B564CB345FFAA6CD031A3C40920E8C3658A3321B864F53252F22D810380C5",
      ],
    );
  } else if (os === "freebsd" && cpu === "aarch64") {
    return new Ok(
      [
        base + "freebsd-arm64/-/freebsd-arm64-0.25.2.tgz",
        "A8B16E6529F098CF7F8855CD2C5FBB21D740534181012AB819A4A569D9EACCDF",
      ],
    );
  } else if (os === "freebsd" && cpu === "amd64") {
    return new Ok(
      [
        base + "freebsd-x64/-/freebsd-x64-0.25.2.tgz",
        "B2394FBF3B85390D5D3246C50192D2B1208D83DBF96796CDC67079C66FC0AA48",
      ],
    );
  } else if (os === "linux" && cpu === "arm") {
    return new Ok(
      [
        base + "linux-arm/-/linux-arm-0.25.2.tgz",
        "9BF3844336FD30A1FCCBB6C0617572518A25E0716F98AC9E293B1D28AF97932D",
      ],
    );
  } else if (os === "linux" && cpu === "aarch64") {
    return new Ok(
      [
        base + "linux-arm64/-/linux-arm64-0.25.2.tgz",
        "87D512B94D322CC3F008DC4EC5D9D47E82001DBEE692B825F332B157AFDF0282",
      ],
    );
  } else if (os === "linux" && cpu === "arm64") {
    return new Ok(
      [
        base + "linux-arm64/-/linux-arm64-0.25.2.tgz",
        "87D512B94D322CC3F008DC4EC5D9D47E82001DBEE692B825F332B157AFDF0282",
      ],
    );
  } else if (os === "linux" && cpu === "ia32") {
    return new Ok(
      [
        base + "linux-ia32/-/linux-ia32-0.25.2.tgz",
        "79BFB8B6B95F32C4EE8BBB21078A9E0F8DED3E148F69EAA690BCC0ABDB8D15F1",
      ],
    );
  } else if (os === "linux" && cpu === "x64") {
    return new Ok(
      [
        base + "linux-x64/-/linux-x64-0.25.2.tgz",
        "52136A4B1F12F8B2567E0550B802F920593B012B22FEC64F46C99DF938466BBA",
      ],
    );
  } else if (os === "linux" && cpu === "x86_64") {
    return new Ok(
      [
        base + "linux-x64/-/linux-x64-0.25.2.tgz",
        "52136A4B1F12F8B2567E0550B802F920593B012B22FEC64F46C99DF938466BBA",
      ],
    );
  } else if (os === "netbsd" && cpu === "x64") {
    return new Ok(
      [
        base + "netbsd-x64/-/netbsd-x64-0.25.2.tgz",
        "0F1DC50A1A688DF5D3E41EDD9DB5B425D9B23566770029ACA4FB6C79D6F1806F",
      ],
    );
  } else if (os === "openbsd" && cpu === "arm64") {
    return new Ok(
      [
        base + "openbsd-arm64/-/openbsd-arm64-0.25.2.tgz",
        "EB8B7F6BB6E56E869DA19573D5088991CA59C12870FC6180F249BA5D34163635",
      ],
    );
  } else if (os === "openbsd" && cpu === "x86_64") {
    return new Ok(
      [
        base + "openbsd-x64/-/openbsd-x64-0.25.2.tgz",
        "196B5E6C9E4EC7895051E49CAF59F5720B9861FE87E66776961C296AE5217A18",
      ],
    );
  } else if (os === "sunos" && cpu === "x64") {
    return new Ok(
      [
        base + "sunos-x64/-/sunos-x64-0.25.2.tgz",
        "409B456C7C341D9EF97621DA4AF84AD1CF0641B0546FE0D5D28A871C78BA2949",
      ],
    );
  } else if (os === "win32" && cpu === "arm64") {
    return new Ok(
      [
        base + "win32-arm64/-/win32-arm64-0.25.2.tgz",
        "CAB0263FFB5CFC6B0609781EA0BF9565F0845C83802B86AB39E2107410911157",
      ],
    );
  } else if (os === "win32" && cpu === "ia32") {
    return new Ok(
      [
        base + "win32-ia32/-/win32-ia32-0.25.2.tgz",
        "F62A2BE084752412CD3B6603668374A844DD3F8C8C37B3EB7382CE3C0F1F1C93",
      ],
    );
  } else if (os === "win32" && cpu === "x64") {
    return new Ok(
      [
        base + "win32-x64/-/win32-x64-0.25.2.tgz",
        "C4D5FD874A44782032FC3BF5FE77D9CCE5D8C5B1DE51D9AE8EE68B40C2FC1ED2",
      ],
    );
  } else if (os === "win32" && cpu === "x86_64") {
    return new Ok(
      [
        base + "win32-x64/-/win32-x64-0.25.2.tgz",
        "C4D5FD874A44782032FC3BF5FE77D9CCE5D8C5B1DE51D9AE8EE68B40C2FC1ED2",
      ],
    );
  } else {
    return new Error(new UnknownPlatform("esbuild", os, cpu));
  }
}

function check_esbuild_integrity(bin, expected_hash) {
  let hash = $crypto.hash(new $crypto.Sha256(), bin);
  let hash_string = $bit_array.base16_encode(hash);
  let $ = hash_string === expected_hash;
  if ($) {
    return new Ok(undefined);
  } else {
    return new Error(new $error.InvalidEsbuildBinary());
  }
}

function write_esbuild(bin, outdir, outfile) {
  let $ = $simplifile.create_directory_all(outdir);
  
  let _pipe = $simplifile.write_bits(outfile, bin);
  return $result.map_error(
    _pipe,
    (_capture) => {
      return new CannotWriteFile(_capture, $filepath.join(outdir, outfile));
    },
  );
}

function set_file_permissions(file) {
  let permissions = new FilePermissions(
    $set.from_list(toList([new Read(), new Write(), new Execute()])),
    $set.from_list(toList([new Read(), new Execute()])),
    $set.from_list(toList([new Read(), new Execute()])),
  );
  let _pipe = $simplifile.set_permissions(file, permissions);
  return $result.map_error(
    _pipe,
    (_capture) => { return new CannotSetPermissions(_capture, file); },
  );
}
