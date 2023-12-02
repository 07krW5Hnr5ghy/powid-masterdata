package com.proyect.masterdata.services.impl;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.SubscriptionRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISubscription;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class SubscriptionImpl implements ISubscription {

    private SubscriptionRepository subscriptionRepository;
    private UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name, Integer months, Double discountPercent)
            throws InternalErrorExceptions, BadRequestExceptions {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'save'");
    }

}
