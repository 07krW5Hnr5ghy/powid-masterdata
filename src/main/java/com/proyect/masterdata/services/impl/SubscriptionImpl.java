package com.proyect.masterdata.services.impl;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.dto.SubscriptionDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.SubscriptionRepository;
import com.proyect.masterdata.repository.SubscriptionRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISubscription;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class SubscriptionImpl implements ISubscription {

    private final SubscriptionRepository subscriptionRepository;
    private final UserRepository userRepository;
    private final SubscriptionRepositoryCustom subscriptionRepositoryCustom;

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
                    .tokenUser(tokenUser.toUpperCase())
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

    @Override
    public Page<SubscriptionDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {

        Page<Subscription> subscriptionPage;

        try {
            subscriptionPage = subscriptionRepositoryCustom.searchForSubscription(name, user, sort, sortColumn,
                    pageNumber, pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.ResultsFound);
        }

        if (subscriptionPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<SubscriptionDTO> subscriptionDTOs = subscriptionPage.getContent().stream()
                .map(subscription -> SubscriptionDTO.builder()
                        .name(subscription.getName().toUpperCase())
                        .months(subscription.getMonths())
                        .discountPercent(subscription.getDiscountPercent())
                        .build())
                .toList();

        return new PageImpl<>(subscriptionDTOs, subscriptionPage.getPageable(), subscriptionPage.getTotalElements());

    }

}
