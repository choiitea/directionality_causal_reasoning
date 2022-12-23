OrigVideoFileName = 'Static Trial - RS_Diamond(Left), GC_Circle(Right) [NoDescend].avi';
WatchOutputRealtime = true; % do you want it to play out in realtime as it writes?
NewFrameRate = 1; % output in frames per second.

%% Import the old video
OrigVideoFile = VideoReader(OrigVideoFileName);

%% Set up a new video to write into
OutputVideoFile = VideoWriter(sprintf('Compressed - %s',OrigVideoFileName), 'Motion JPEG AVI');
OutputVideoFile.FrameRate = NewFrameRate; % frames per second
OutputVideoFile.Quality = 100; % NO loss of quality in JPEG compression, set to 75 for tiny artifacts but smaller file
open(OutputVideoFile);

%% Step through each frame of the old video and write to output video
frameCounter = 1;
figure();

while hasFrame(OrigVideoFile)

    % Read in a frame
    thisFrame = readFrame(OrigVideoFile);
    writeFrame = ~mod(frameCounter,round(OrigVideoFile.FrameRate/OutputVideoFile.FrameRate));
    fprintf('Frame: %g, FR: %g, remainder: %g\n', frameCounter, OrigVideoFile.FrameRate, writeFrame);
    if writeFrame
        if WatchOutputRealtime
            imshow(thisFrame);
        end
        title(sprintf('Writing frame %g to output video',frameCounter));
        writeVideo(OutputVideoFile,thisFrame);
        fprintf('Wrote video frame.\n')
    end
    if WatchOutputRealtime
        pause(1/OrigVideoFile.FrameRate);
    end
    frameCounter = frameCounter + 1;
end
close(OutputVideoFile);