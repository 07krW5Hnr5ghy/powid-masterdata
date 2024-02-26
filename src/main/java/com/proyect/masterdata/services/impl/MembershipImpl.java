package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.dto.MembershipDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IMembership;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Date;

@Service
@RequiredArgsConstructor
@Log4j2
public class MembershipImpl implements IMembership {

    private final MembershipRepository membershipRepository;
    private final UserRepository userRepository;
    private final ModuleRepository moduleRepository;
    private final ClientRepository clientRepository;
    private final MembershipRepositoryCustom membershipRepositoryCustom;
    private final SubscriptionRepository subscriptionRepository;

    @Override
    public ResponseSuccess save(Client client, MembershipPayment membershipPayment, String subscriptionName,List<String> modules, Boolean demo, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        Subscription subscription;

        try {
            subscription = subscriptionRepository.findByNameAndStatusTrue(subscriptionName.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (client == null) {
            throw new BadRequestExceptions(Constants.ErrorClient);
        }

        if (subscription == null) {
            throw new BadRequestExceptions(Constants.ErrorSubscription);
        }

        try {

            Date currentDate = new Date(System.currentTimeMillis());

            Calendar calendar = Calendar.getInstance();

            calendar.setTime(currentDate);

            calendar.add(calendar.MONTH, subscription.getMonths());

            Date expirationDate = calendar.getTime();

            membershipRepository.save(Membership.builder()
                            .clientId(client.getId())
                            .client(client)
                            .demo(demo)
                            .expirationDate(expirationDate)
                            .membershipPayment(membershipPayment)
                            .membershipPaymentId(membershipPayment.getId())
                            .status(true)
                            .updateDate(new Date(System.currentTimeMillis()))
                            .subscription(subscription)
                            .subscriptionId(subscription.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
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
    public ResponseDelete delete(String clientRuc, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        Client client;
        Membership membership = null;

        try {

            existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
            client = clientRepository.findByRucAndStatusTrue(clientRuc);

            if (client != null) {
                membership = membershipRepository.findByClientIdAndStatusTrue(client.getId());
            }

        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (client == null) {
            throw new BadRequestExceptions(Constants.ErrorClient);
        }

        if (membership == null) {
            throw new BadRequestExceptions(Constants.ErrorMembership);
        }

        try {

            Date currentDate = new Date(System.currentTimeMillis());

            // if (currentDate.after(membership.getExpirationDate())
            // || currentDate.equals(membership.getExpirationDate())) {

            // membership.setStatus(false);
            // membership.setUpdateDate(currentDate);

            // } else {
            // throw new BadRequestExceptions(Constants.ErrorMembershipNotExpired);
            // }

            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();

        } catch (

        RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<MembershipDTO> list(String channel, String module, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<Membership> membershipPage = null;
        Module moduleData;
        try {
            moduleData = moduleRepository.findByNameAndStatusTrue(module.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (membershipPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        List<MembershipDTO> membershipDTOS = membershipPage.getContent().stream().map(membership -> {
            return MembershipDTO.builder()
                    .build();
        }).toList();
        return new PageImpl<>(membershipDTOS, membershipPage.getPageable(), membershipPage.getTotalElements());
    }

}
