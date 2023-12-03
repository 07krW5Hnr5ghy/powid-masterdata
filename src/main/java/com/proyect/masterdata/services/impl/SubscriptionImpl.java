package com.proyect.masterdata.services.impl;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.SubscriptionRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISubscription;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import java.util.Date;

@Service
@RequiredArgsConstructor
@Log4j2
public class SubscriptionImpl implements ISubscription {

    private final SubscriptionRepository subscriptionRepository;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name, Integer months, Double discountPercent, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        boolean existsSubscription;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsSubscription = subscriptionRepository.existsByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsSubscription) {
            throw new BadRequestExceptions(Constants.ErrorSubscriptionExists);
        }

        try {
            subscriptionRepository.save(Subscription.builder()
                    .name(name.toUpperCase())
                    .months(months)
                    .discountPercent(discountPercent)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .build());

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

}
