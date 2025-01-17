/*
 * Copyright (c) 2021 Broadcom.
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

import * as path from "path";
import * as vscode from "vscode";
import { ProfileUtils } from "../../../services/util/ProfileUtils";

function getZoweExplorerMock(): IApiRegisterClient {
  return {
    getUssApi: jest.fn(),
    getMvsApi: jest.fn(),
    getExplorerExtenderApi: () => ({
      getProfilesCache: () => ({
        getProfiles: () => [
          {
            name: "profile",
            profile: { encoding: "" },
          },
          {
            name: "profile2",
            profile: { encoding: "" },
          },
        ],
        loadNamedProfile: () => ({
          name: "profile",
          profile: { encoding: "" },
        }),
      }),
      ussFileProvider: {
        openFiles: {
          "COBOLFI2.cbl": {
            profile: {
              name: "profile-1",
            },
          },
        },
      },
      datasetProvider: {
        openFiles: {
          "COBOLFILE.cbl": {
            profile: {
              name: "profile-1",
            },
          },
        },
      },
    }),
    registeredApiTypes: () => ["zosmf"],
  };
}

describe("Test profile Utils", () => {
  const programName = "COBOLFILE.cbl";
  const profile = "profile";
  it("checks a profile passed through settings is always given preference over profile from doc path for copybook download", () => {
    const zoweApiMock = getZoweExplorerMock();
    vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
      get: jest.fn().mockReturnValue("profileInSettings"),
    });
    ProfileUtils.getAvailableProfiles = () => [profile];
    expect(
      ProfileUtils.getProfileNameForCopybook(programName, zoweApiMock),
    ).toBe("profileInSettings");
  });

  it("checks that profile is fetched from the settings if not a ZE downloaded file", () => {
    const zoweApiMock = getZoweExplorerMock();
    vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
      get: jest.fn().mockReturnValue("profile2"),
    });
    expect(
      ProfileUtils.getProfileNameForCopybook(programName, zoweApiMock),
    ).toBe("profile2");
  });
  it("test zowe v3 profile extraction", () => {
    vscode.Uri.parse = jest.fn().mockImplementation((arg: string) => {
      const match = /^([^:]+):(.*)/.exec(arg);
      return {
        scheme: match?.[1],
        path: match?.[2],
        fsPath: match?.[2]?.replace("/", path.sep),
      };
    });
    expect(ProfileUtils.getProfileFromDocument("", undefined)).toBeUndefined();
    expect(
      ProfileUtils.getProfileFromDocument("zowe-ds:", undefined),
    ).toBeUndefined();
    expect(
      ProfileUtils.getProfileFromDocument("zowe-ds:/", undefined),
    ).toBeUndefined();
    expect(
      ProfileUtils.getProfileFromDocument("zowe-ds:/profile", undefined),
    ).toBe("profile");
  });
});
