package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.MembershipPayment;
import com.proyect.masterdata.dto.MembershipPaymentDTO;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.concurrent.CompletableFuture;

public interface IMembershipPayment {
        CompletableFuture<ResponseSuccess> save(RequestMembershipPayment requestMembershipPayment, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<MembershipPaymentDTO>> list(String user, Double grossAmount, Double netAmount, Double paymentGatewayFee, Double taxAmount, String paymentGateway, String sort, String sortColumn,
                                        Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
}
