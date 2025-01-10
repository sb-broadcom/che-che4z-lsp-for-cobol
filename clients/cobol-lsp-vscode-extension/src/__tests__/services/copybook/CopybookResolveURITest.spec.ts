/*
 * Copyright (c) 2020 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *   Broadcom, Inc. - initial API and implementation
 */
import * as glob from "glob";
import * as fs from "fs-extra";
import * as path from "path";
import * as vscode from "vscode";
import { COPYBOOK_EXT_ARRAY, ZOWE_FOLDER } from "../../../constants";
import { CopybookURI } from "../../../services/copybook/CopybookURI";
import { SettingsService } from "../../../services/Settings";
import * as fsUtils from "../../../services/util/FSUtils";
import { ProfileUtils } from "../../../services/util/ProfileUtils";
import { SettingsUtils } from "../../../services/util/SettingsUtils";
import { CopybookDownloadService } from "../../../services/copybook/CopybookDownloadService";

const copybookName: string = "NSTCOPY1";
const copybookNameWithExtension: string = "NSTCOPY2.CPY";
const CPY_FOLDER_NAME = ".cobcopy";
const RELATIVE_CPY_FOLDER_NAME = "../relativeCobcopy";
const folderPath = path.join(__dirname, CPY_FOLDER_NAME);

SettingsUtils.getWorkspaceFoldersPath = jest.fn().mockReturnValue([__dirname]);
vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
  get: jest.fn().mockReturnValue(undefined),
});

// file utils
function createFile(filename: string, folderPath: string): string {
  fs.writeFileSync(path.join(folderPath, filename), "Some dummy content");
  return path.resolve(folderPath, filename);
}

function createDirectory(targetPath: string) {
  fs.mkdirSync(targetPath, { recursive: true });
}

function removeFolder(targetPath: string) {
  if (fs.existsSync(targetPath)) {
    return fs.remove(targetPath);
  }
  return false;
}

function buildResultArrayFrom(
  settingsMockValue: string[] | undefined,
  filename: string,
  profileName: string | undefined,
  ussPath: string[] = [],
): number {
  vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
    get: jest.fn().mockReturnValueOnce(settingsMockValue),
  });
  if (ussPath.length > 0) {
    vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
      get: jest.fn().mockReturnValue(ussPath),
    });
  }
  ProfileUtils.getProfileNameForCopybook = jest
    .fn()
    .mockImplementation(() => profileName);
  const result = CopybookURI.createPathForCopybookDownloaded(
    filename,
    SettingsService.DEFAULT_DIALECT,
    path.join("downloadFolder", ZOWE_FOLDER),
    {} as unknown as IApiRegisterClient,
  );
  return result.length;
}
beforeEach(() => {
  jest.clearAllMocks();
});
beforeAll(() => {
  createDirectory(folderPath);
  createFile(copybookName, folderPath);
  createFile(copybookNameWithExtension, folderPath);
});
afterAll(() => {
  return removeFolder(folderPath);
});
describe("Resolve local copybook against bad configuration of target folders", () => {
  test("given an empty list of folders, the copybook is not retrieved", () => {
    expect(
      fsUtils.searchCopybookInExtensionFolder(
        copybookName,
        [],
        COPYBOOK_EXT_ARRAY,
        __dirname,
      ),
    ).toBe(undefined);
  });
  test("given a folder that not contains copybooks, the target copybook is not retrieved", () => {
    jest.spyOn(glob, "globSync").mockImplementation(() => []);
    expect(
      fsUtils.searchCopybookInExtensionFolder(
        copybookName,
        [__dirname],
        COPYBOOK_EXT_ARRAY,
        __dirname,
      ),
    ).toBe(undefined);
  });
  test("given a not empty folder, a copybook that is not present in that folder is not retrieved and the uri returned is undefined", () => {
    jest.spyOn(glob, "globSync").mockImplementation(() => []);
    expect(
      fsUtils.searchCopybookInExtensionFolder(
        "NSTCPY2",
        [CPY_FOLDER_NAME],
        COPYBOOK_EXT_ARRAY,
        __dirname,
      ),
    ).toBeUndefined();
  });
});
describe("Resolve local copybook present in one or more folders specified by the user", () => {
  test("given a folder that contains the target copybook, it is found and its uri is returned", () => {
    jest.spyOn(glob, "globSync").mockImplementation(() => [copybookName]);
    expect(
      fsUtils.searchCopybookInExtensionFolder(
        copybookName,
        [CPY_FOLDER_NAME],
        COPYBOOK_EXT_ARRAY,
        __dirname,
      ),
    ).toBeDefined();
  });
  test("given two times the same folder that contains the target copybook, one uri is still returned", () => {
    jest.spyOn(glob, "globSync").mockImplementation(() => [copybookName]);
    expect(
      fsUtils.searchCopybookInExtensionFolder(
        copybookName,
        [CPY_FOLDER_NAME],
        COPYBOOK_EXT_ARRAY,
        __dirname,
      ),
    ).toBeDefined();
  });
  test("Given a copybook with extension on filesystem, the uri is correctly returned", () => {
    jest.spyOn(glob, "globSync").mockImplementation(() => ["NSTCOPY2.CPY"]);
    expect(
      fsUtils.searchCopybookInExtensionFolder(
        "NSTCOPY2",
        [CPY_FOLDER_NAME],
        COPYBOOK_EXT_ARRAY,
        __dirname,
      ),
    ).toBeDefined();
  });
  test("Given a valid relative path for copybook with extension on filesystem, the uri is correctly returned", async () => {
    jest.spyOn(glob, "globSync").mockImplementation(() => ["NSTCOPY2.CPY"]);
    const dir = path.join(__dirname, RELATIVE_CPY_FOLDER_NAME);
    createDirectory(dir);
    createFile(copybookNameWithExtension, dir);
    expect(
      fsUtils.searchCopybookInExtensionFolder(
        "NSTCOPY2",
        [RELATIVE_CPY_FOLDER_NAME],
        COPYBOOK_EXT_ARRAY,
        __dirname,
      ),
    ).toBeDefined();
    await removeFolder(dir);
  });
  test("Given a valid absolute path for copybook with extension on filesystem, the uri is correctly returned", () => {
    jest.spyOn(glob, "globSync").mockImplementation(() => ["NSTCOPY2.CPY"]);
    expect(
      fsUtils.searchCopybookInExtensionFolder(
        "NSTCOPY2",
        [path.normalize(folderPath)],
        COPYBOOK_EXT_ARRAY,
        __dirname,
      ),
    ).toBeDefined();
  });
});
describe("With invalid input parameters, the list of URI that represent copybook downloaded are not generated", () => {
  test("given a profile but no dataset, the result list returned is empty", () => {
    expect(buildResultArrayFrom(undefined, "file", "PRF")).toBe(0);
  });
  test("given a list of dataset but no profile, the result list returned is empty", () => {
    expect(
      buildResultArrayFrom(["HLQ.DATASET1.DATASET2"], "file", undefined),
    ).toBe(0);
  });
});
describe("With allowed input parameters, the list of URI that represent copybook downloaded is correctly generated", () => {
  test("given profile and dataset list with one element, the result list is correctly generated with size 1 ", () => {
    expect(buildResultArrayFrom(["HLQ.DATASET1.DATASET2"], "file", "PRF")).toBe(
      1,
    );
  });
  test("given profile, dataset and USS path, list with one element each, the result list is correctly generated with size 2 ", () => {
    expect(
      buildResultArrayFrom(["HLQ.DATASET1.DATASET2"], "file", "PRF", [
        "/test/uss/path",
      ]),
    ).toBe(2);
  });
});
describe("Prioritize search criteria for copybooks test suite", () => {
  let settingsMockProperties: Record<string, unknown> = {};
  let spySearchInWorkspace: jest.SpyInstance<
    ReturnType<typeof fsUtils.searchCopybookInExtensionFolder>,
    Parameters<typeof fsUtils.searchCopybookInExtensionFolder>
  >;

  let globSyncMockResult: string[] = [];

  beforeEach(() => {
    jest.spyOn(vscode.workspace, "getConfiguration").mockReturnValue({
      get: (key: string) => settingsMockProperties[key],
    } as unknown as vscode.WorkspaceConfiguration);

    spySearchInWorkspace = jest.spyOn(
      fsUtils,
      "searchCopybookInExtensionFolder",
    );

    jest.spyOn(glob, "globSync").mockImplementation(() => globSyncMockResult);
  });

  test("With only a local folder defined in the settings.json, the search is applied locally", async () => {
    settingsMockProperties = {
      "paths-local": [CPY_FOLDER_NAME],
      "copybook-extensions": [""],
    };
    globSyncMockResult = [copybookName];

    const downloader = new CopybookDownloadService("/storagePath");
    const uri: string | undefined = await downloader.resolveCopybookHandler(
      copybookName,
      "PRGNAME",
      "COBOL",
    );
    expect(uri).toMatch(CPY_FOLDER_NAME);
    expect(spySearchInWorkspace).toHaveBeenCalledTimes(1);
  });

  test("With no settings provided, two search strategies are applied and function return undefined", async () => {
    globSyncMockResult = [];
    settingsMockProperties = {};

    ProfileUtils.getProfileNameForCopybook = jest
      .fn()
      .mockReturnValue(undefined);
    const downloader = new CopybookDownloadService(
      "/storagePath",
      undefined,
      undefined,
    );
    const uri: string | undefined = await downloader.resolveCopybookHandler(
      copybookName,
      "PRGNAME",
      "COBOL",
    );
    expect(uri).toBe(undefined);

    expect(spySearchInWorkspace).toHaveBeenCalledTimes(7);
  });

  test("With both local and dsn references defined in the settings.json, the search is applied on local resources first", async () => {
    globSyncMockResult = [];
    settingsMockProperties = {
      "paths-local": [CPY_FOLDER_NAME],
      "paths-dsn": ["DATASET.WITH.COPYBOOK"],
      profiles: "zosmf",
      "copybook-extensions": [""],
    };
    const downloader = new CopybookDownloadService("/storagePath");

    const uri: string | undefined = await downloader.resolveCopybookHandler(
      copybookName,
      "PRGNAME",
      "COBOL",
    );
    expect(uri).not.toBe("");
    expect(spySearchInWorkspace).toHaveBeenCalledTimes(7);

    // check that first call searching in local folder
    expect(spySearchInWorkspace.mock.calls[0][1]![0]).toEqual(
      expect.stringContaining(CPY_FOLDER_NAME),
    );

    // check that call searching in dsn folder is second
    expect(spySearchInWorkspace.mock.calls[1][1]![0]).toEqual(
      expect.stringContaining("DATASET.WITH.COPYBOOK"),
    );
  });

  test("With only a local folder defined for the dialect in the settings.json, the search is applied locally", async () => {
    settingsMockProperties = {
      "dialect.paths-local": [CPY_FOLDER_NAME],
      "copybook-extensions": [""],
    };
    globSyncMockResult = [copybookName];

    const downloader = new CopybookDownloadService("/storagePath");
    const uri: string | undefined = await downloader.resolveCopybookHandler(
      copybookName,
      "PRGNAME",
      "DIALECT",
    );
    expect(uri).toMatch(CPY_FOLDER_NAME);
    expect(spySearchInWorkspace).toHaveBeenCalledTimes(1);
  });
});
