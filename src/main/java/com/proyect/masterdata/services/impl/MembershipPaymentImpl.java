package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.domain.PaymentGateway;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PaymentUpdateDTO;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
import com.proyect.masterdata.dto.request.RequestMembershipPaymentUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IMembership;
import com.proyect.masterdata.services.IMembershipPayment;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;

@Service
@RequiredArgsConstructor
@Log4j2
public class MembershipPaymentImpl implements IMembershipPayment {

    private final MembershipPaymentRepository membershipPaymentRepository;
    private final MembershipRepository membershipRepository;
    private final UserRepository userRepository;
    private final MembershipPaymentRepositoryCustom membershipPaymentRepositoryCustom;
    private final OrderPaymentMethodRepository orderPaymentMethodRepository;
    private final OrderPaymentStateRepository orderPaymentStateRepository;
    private final IMembership iMembership;
    private final PaymentGatewayRepository paymentGatewayRepository;
    @Override
    public ResponseSuccess save(RequestMembershipPayment requestMembershipPayment, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        PaymentGateway paymentGateway;
        MembershipPayment membershipPayment;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            paymentGateway = paymentGatewayRepository.findByNameAndStatusTrue(requestMembershipPayment.getPaymentGateway().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try {

            MembershipPayment newMembershipPayment = membershipPaymentRepository.save(MembershipPayment.builder()
                    .netAmount(requestMembershipPayment.getNetAmount())
                    .grossAmount(requestMembershipPayment.getGrossAmount())
                    .taxAmount(requestMembershipPayment.getTaxAmount())
                    .paymentGatewayFee(requestMembershipPayment.getPaymentGatewayFee())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .updateDate(new Date(System.currentTimeMillis()))
                    .paymentGateway(paymentGateway)
                    .paymentGatewayId(paymentGateway.getId())
                    .build());

            iMembership.save(user.getClient(),newMembershipPayment, requestMembershipPayment.getSubscriptionName(), requestMembershipPayment.getModules(),requestMembershipPayment.getDemo(),user.getUsername());

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
    public PaymentUpdateDTO update(Long membershipId, RequestMembershipPaymentUpdate requestMembershipPaymentUpdate,
            String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        MembershipPayment membershipPayment;
        boolean existsUser;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try {
            // payment.setIdPaymentState(newState.getId());
            // if (requestPaymentUpdate.getNewInvoiceUrl() != null
            // & requestPaymentUpdate.getNewInvoiceUrl() != payment.getUrlInvoice()) {
            // payment.setUrlInvoice(requestPaymentUpdate.getNewInvoiceUrl());
            // }
            // payment.setDateRegistration(new Date(System.currentTimeMillis()));
            // payment = paymentRepository.save(payment);
            return PaymentUpdateDTO.builder()
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<MembershipPayment> list(Double totalPayment, String month, String channel, String sort,
            String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<MembershipPayment> paymentPage;

        try {
            paymentPage = membershipPaymentRepositoryCustom.searchForPayment(totalPayment, month, null, sort,
                    sortColumn, pageNumber, pageSize);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (paymentPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        return new PageImpl<>(paymentPage.getContent(),
                paymentPage.getPageable(), paymentPage.getTotalElements());
    }

}
