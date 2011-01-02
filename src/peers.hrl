
-record(peer, {
        % from request
        id,                 % public id
        key,                % private key, as ip is not used for auth
        ip,                 % peer ip
        port,               % peer port
        left,               % bytes left
        downloaded,         % bytes downloaded
        downloaded_prev,    % bytes downloaded, previous request
        uploaded,           % bytes uploaded
        uploaded_prev,      % bytes uploaded, previous request

        last_seen,          % time of last request
        last_seen_prev,     % time of previous request
        natcheck            % undefined or failed
        }).

