package com.proyect.masterdata.services.impl;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.OnboardStore;
import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OnboardStoreRepository;
import com.proyect.masterdata.services.IOnboardStore;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class OnboardStoreImpl implements IOnboardStore {

    private final OnboardStoreRepository onboardStoreRepository;

    @Override
    public OnboardStore save(Store store, Onboard onboard) throws InternalErrorExceptions {

        try {
            return onboardStoreRepository.save(OnboardStore.builder()
                    .onboard(onboard)
                    .onboardId(onboard.getId())
                    .store(store)
                    .storeId(store.getId())
                    .build());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
