package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.domain.PaymentGateway;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.MembershipPaymentDTO;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
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
import java.util.List;

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

            iMembership.save(user,newMembershipPayment, requestMembershipPayment.getSubscriptionName(), requestMembershipPayment.getModules(),requestMembershipPayment.getDemo(),user.getUsername());

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
    public Page<MembershipPaymentDTO> list(String user, Double grossAmount, Double netAmount, Double paymentGatewayFee, Double taxAmount, String paymentGateway, String sort,
                                           String sortColumn,
                                           Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<MembershipPayment> membershipPaymentPage;
        Long clientId;
        Long paymentGatewayId;

        if(paymentGateway != null){
            paymentGatewayId = paymentGatewayRepository.findByNameAndStatusTrue(paymentGateway.toUpperCase()).getId();
        }else{
            paymentGatewayId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            membershipPaymentPage = membershipPaymentRepositoryCustom.searchForMembershipPayment(clientId, grossAmount, netAmount, paymentGatewayFee, taxAmount,paymentGatewayId, sort,
                    sortColumn, pageNumber, pageSize);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (membershipPaymentPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<MembershipPaymentDTO> membershipPaymentDTOS = membershipPaymentPage.getContent().stream().map(membershipPayment -> MembershipPaymentDTO.builder()
                .grossAmount(membershipPayment.getGrossAmount())
                .netAmount(membershipPayment.getNetAmount())
                .paymentGatewayFee(membershipPayment.getPaymentGatewayFee())
                .taxAmount(membershipPayment.getTaxAmount())
                .paymentGateway(membershipPayment.getPaymentGateway().getName())
                .registrationDate(membershipPayment.getRegistrationDate())
                .build()).toList();

        return new PageImpl<>(membershipPaymentDTOS,
                membershipPaymentPage.getPageable(), membershipPaymentPage.getTotalElements());
    }

}
