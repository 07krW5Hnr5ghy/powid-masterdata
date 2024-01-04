package com.proyect.masterdata.services.impl;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.domain.OnboardModule;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OnboardModuleRepository;
import com.proyect.masterdata.services.IOnboardModule;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class OnboardModuleImpl implements IOnboardModule {

    private final OnboardModuleRepository onboardModuleRepository;

    @Override
    public OnboardModule save(Onboard onboard, Module module) throws InternalErrorExceptions {
        try {
            OnboardModule onboardModule = OnboardModule.builder()
                    .module(module)
                    .moduleId(module.getId())
                    .onboard(onboard)
                    .onboardId(onboard.getId())
                    .build();
            return onboardModuleRepository.save(onboardModule);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
