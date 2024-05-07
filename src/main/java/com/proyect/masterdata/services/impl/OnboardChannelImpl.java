package com.proyect.masterdata.services.impl;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.ClosingChannel;
import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.OnboardChannel;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OnboardChannelRepository;
import com.proyect.masterdata.services.IOnboardChannel;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OnboardChannelImpl implements IOnboardChannel {

    private final OnboardChannelRepository onboardChannelRepository;

    @Override
    public CompletableFuture<OnboardChannel> save(Onboard onboard, ClosingChannel closingChannel) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {

                OnboardChannel onboardChannel = OnboardChannel.builder()
                        .closingChannel(closingChannel)
                        .closingChannelId(closingChannel.getId())
                        .onboard(onboard)
                        .onboardId(onboard.getId())
                        .build();

                return onboardChannelRepository.save(onboardChannel);

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

}
