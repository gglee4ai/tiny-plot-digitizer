import AppKit
import Darwin
import Foundation

private final class AppDelegate: NSObject, NSApplicationDelegate {
    private let defaultPort = 8837
    private var serverProcess: Process?
    private var logHandle: FileHandle?
    private var isLaunching = false
    private var isTerminating = false
    private var serverReady = false
    private var signalSources: [DispatchSourceSignal] = []

    private lazy var port: Int = {
        guard
            let value = ProcessInfo.processInfo.environment["DIGITIZER_PORT"],
            let parsed = Int(value),
            (1...65_535).contains(parsed)
        else {
            return defaultPort
        }
        return parsed
    }()

    private var appURL: URL {
        URL(string: "http://127.0.0.1:\(port)/")!
    }

    private var shouldOpenBrowser: Bool {
        let setting = ProcessInfo.processInfo.environment["DIGITIZER_BROWSER"]?
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .lowercased() ?? "true"
        return !["0", "false", "no", "off"].contains(setting)
    }

    func applicationDidFinishLaunching(_ notification: Notification) {
        installMainMenu()
        installSignalHandlers()
        ensureServer()
    }

    func applicationShouldHandleReopen(
        _ sender: NSApplication,
        hasVisibleWindows flag: Bool
    ) -> Bool {
        ensureServer()
        return true
    }

    func applicationWillTerminate(_ notification: Notification) {
        isTerminating = true
        if let process = serverProcess, process.isRunning {
            process.terminate()
        }
        try? logHandle?.close()
    }

    private func installMainMenu() {
        let mainMenu = NSMenu()
        let appMenuItem = NSMenuItem()
        let appMenu = NSMenu()
        appMenu.addItem(
            withTitle: "Tiny Plot Digitizer 종료",
            action: #selector(NSApplication.terminate(_:)),
            keyEquivalent: "q"
        )
        appMenuItem.submenu = appMenu
        mainMenu.addItem(appMenuItem)
        NSApplication.shared.mainMenu = mainMenu
    }

    private func installSignalHandlers() {
        for signalNumber in [SIGINT, SIGTERM] {
            signal(signalNumber, SIG_IGN)
            let source = DispatchSource.makeSignalSource(
                signal: signalNumber,
                queue: .main
            )
            source.setEventHandler {
                NSApplication.shared.terminate(nil)
            }
            source.resume()
            signalSources.append(source)
        }
    }

    private func ensureServer() {
        if serverReady {
            openBrowser()
            return
        }
        guard !isLaunching else { return }
        isLaunching = true

        serverIsReady { [weak self] ready in
            guard let self else { return }
            if ready {
                self.finishLaunching()
            } else {
                self.startServer()
            }
        }
    }

    private func startServer() {
        guard
            let resources = Bundle.main.resourceURL,
            FileManager.default.fileExists(
                atPath: resources.appendingPathComponent("app/run.R").path
            )
        else {
            failLaunching("앱 번들에서 run.R을 찾을 수 없습니다.")
            return
        }

        guard let rscript = findRscript() else {
            failLaunching("Rscript를 찾을 수 없습니다. 먼저 R을 설치하세요.")
            return
        }

        do {
            let handle = try prepareLogHandle()
            let process = Process()
            process.executableURL = rscript
            process.arguments = [
                resources.appendingPathComponent("app/run.R").path
            ]
            var environment = ProcessInfo.processInfo.environment
            environment["LANG"] = "en_US.UTF-8"
            environment["LC_ALL"] = "en_US.UTF-8"
            environment["DIGITIZER_BROWSER"] = "false"
            environment["DIGITIZER_PORT"] = String(port)
            environment["PATH"] = [
                "/opt/homebrew/bin",
                "/usr/local/bin",
                "/usr/bin",
                "/bin",
                "/usr/sbin",
                "/sbin"
            ].joined(separator: ":")
            process.environment = environment
            process.standardOutput = handle
            process.standardError = handle
            process.terminationHandler = { [weak self] process in
                DispatchQueue.main.async {
                    guard let self, !self.isTerminating else { return }
                    if self.serverReady {
                        self.serverReady = false
                        self.showError(
                            "앱 서버가 종료되었습니다. 로그를 확인하세요: "
                                + self.logFileURL.path
                        )
                    }
                }
            }
            try process.run()
            serverProcess = process
            waitForServer(attempt: 0)
        } catch {
            failLaunching("앱을 실행하지 못했습니다: \(error.localizedDescription)")
        }
    }

    private func waitForServer(attempt: Int) {
        serverIsReady { [weak self] ready in
            guard let self else { return }
            if ready {
                self.finishLaunching()
                return
            }
            if attempt >= 100 || self.serverProcess?.isRunning == false {
                self.failLaunching(
                    "앱 서버를 시작하지 못했습니다. 로그를 확인하세요: "
                        + self.logFileURL.path
                )
                return
            }
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) {
                self.waitForServer(attempt: attempt + 1)
            }
        }
    }

    private func finishLaunching() {
        isLaunching = false
        serverReady = true
        openBrowser()
    }

    private func failLaunching(_ message: String) {
        isLaunching = false
        showError(message)
    }

    private func openBrowser() {
        guard shouldOpenBrowser else { return }
        NSWorkspace.shared.open(appURL)
    }

    private func serverIsReady(completion: @escaping (Bool) -> Void) {
        var request = URLRequest(
            url: appURL,
            cachePolicy: .reloadIgnoringLocalAndRemoteCacheData,
            timeoutInterval: 0.8
        )
        request.httpMethod = "GET"
        URLSession.shared.dataTask(with: request) { data, response, _ in
            let status = (response as? HTTPURLResponse)?.statusCode ?? 0
            let html = data.flatMap { String(data: $0, encoding: .utf8) } ?? ""
            let ready = (200..<400).contains(status)
                && html.contains("Tiny Plot Digitizer")
            DispatchQueue.main.async {
                completion(ready)
            }
        }.resume()
    }

    private func findRscript() -> URL? {
        let candidates = [
            "/Library/Frameworks/R.framework/Resources/bin/Rscript",
            "/opt/homebrew/bin/Rscript",
            "/usr/local/bin/Rscript"
        ]
        return candidates.first(where: {
            FileManager.default.isExecutableFile(atPath: $0)
        }).map(URL.init(fileURLWithPath:))
    }

    private var logFileURL: URL {
        FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent("Library/Logs/Tiny Plot Digitizer.log")
    }

    private func prepareLogHandle() throws -> FileHandle {
        let url = logFileURL
        try FileManager.default.createDirectory(
            at: url.deletingLastPathComponent(),
            withIntermediateDirectories: true
        )
        if !FileManager.default.fileExists(atPath: url.path) {
            FileManager.default.createFile(atPath: url.path, contents: nil)
        }
        let handle = try FileHandle(forWritingTo: url)
        try handle.seekToEnd()
        logHandle = handle
        return handle
    }

    private func showError(_ message: String) {
        let alert = NSAlert()
        alert.messageText = "Tiny Plot Digitizer"
        alert.informativeText = message
        alert.alertStyle = .critical
        alert.runModal()
    }
}

@main
private struct TinyPlotDigitizerLauncher {
    static func main() {
        let application = NSApplication.shared
        let delegate = AppDelegate()
        application.setActivationPolicy(.regular)
        application.delegate = delegate
        application.run()
    }
}
