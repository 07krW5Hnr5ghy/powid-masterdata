package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.PaymentDTO;
import com.proyect.masterdata.dto.PaymentUpdateDTO;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
import com.proyect.masterdata.dto.request.RequestPaymentUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.PaymentMapper;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IMembershipPayment;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class MembershipPaymentImpl implements IMembershipPayment {
    private final MembershipPaymentRepository membershipPaymentRepository;
    private final MembershipRepository membershipRepository;
    private final UserRepository userRepository;
    private final ChannelRepository channelRepository;
    private final MembershipPaymentRepositoryCustom membershipPaymentRepositoryCustom;
    private final MembershipPayment membershipPayment;

    @Override
    public ResponseSuccess save(Long membershipId, RequestMembershipPayment requestMembershipPayment, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        Membership membership;
        MembershipPayment membershipPayment;

        try {
            existsUser = userRepository.existsByUsername(tokenUser.toUpperCase());
            membership = membershipRepository.findByIdAndStatusTrue(membershipId);
            membershipPayment = membershipPaymentRepository.findByStatusTrue();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (membership == null) {
            throw new BadRequestExceptions(Constants.ErrorMembership);
        }

        if (membershipPayment != null) {
            throw new BadRequestExceptions(Constants.ErrorMembershipPaymentExist);
        }

        try {

            membershipPaymentRepository.save(MembershipPayment.builder()
                    .netAmount(requestMembershipPayment.getNetAmount())
                    .grossAmount(requestMembershipPayment.getGrossAmount())
                    .months(requestMembershipPayment.getMonths())
                    .invoiceUrl(requestMembershipPayment.getInvoiceUrl())
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

    @Override
    public PaymentUpdateDTO update(Long membershipId, String invoiceUrl, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        MembershipPayment payment;
        boolean existsUser;
        Channel channel;
        PaymentState paymentState;
        PaymentState newState;

        try {
            existsUser = userRepository.existsByUsername(tokenUser.toUpperCase());
            channel = channelRepository.findByName(requestPaymentUpdate.getChannel().toUpperCase());
            paymentState = paymentStateRepository
                    .findByNameAndStatusTrue(requestPaymentUpdate.getPaymentState().toUpperCase());
            newState = paymentStateRepository.findByNameAndStatusTrue(newPaymentState.toUpperCase());
            payment = paymentRepository.findByIdChannelAndMonthAndIdPaymentState(channel.getId(),
                    requestPaymentUpdate.getMonth().toUpperCase(), paymentState.getId());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (channel == null) {
            throw new BadRequestExceptions("Canal no existe");
        }

        if (paymentState == null) {
            throw new BadRequestExceptions("Estado de pago no existe");
        }

        if (newState == null) {
            throw new BadRequestExceptions("Nuevo estado de pago no existe");
        }
        if (payment == null) {
            throw new BadRequestExceptions("Pago no existe");
        }

        try {
            payment.setIdPaymentState(newState.getId());
            if (requestPaymentUpdate.getNewInvoiceUrl() != null
                    & requestPaymentUpdate.getNewInvoiceUrl() != payment.getUrlInvoice()) {
                payment.setUrlInvoice(requestPaymentUpdate.getNewInvoiceUrl());
            }
            payment.setDateRegistration(new Date(System.currentTimeMillis()));
            payment = paymentRepository.save(payment);
            return PaymentUpdateDTO.builder()
                    .totalPayment(payment.getIdPaymentState())
                    .discount(payment.getDiscount())
                    .channel(payment.getChannel().getName().toUpperCase())
                    .month(payment.getMonth().toUpperCase())
                    .urlInvoice(payment.getUrlInvoice())
                    .paymentState(newState.getName().toUpperCase())
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
        Channel channelData;
        try {
            if (channel != null) {
                channelData = channelRepository.findByName(channel.toUpperCase());
                paymentPage = paymentRepositoryCustom.searchForPayment(totalPayment, month, channelData.getId(), sort,
                        sortColumn, pageNumber, pageSize);
            } else {
                paymentPage = paymentRepositoryCustom.searchForPayment(totalPayment, month, null, sort, sortColumn,
                        pageNumber, pageSize);
            }
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
