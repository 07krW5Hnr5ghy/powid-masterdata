package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.OrderPaymentMethod;
import com.proyect.masterdata.domain.PaymentMetodClient;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ClientRepository;
import com.proyect.masterdata.repository.OrderPaymentMethodRepository;
import com.proyect.masterdata.repository.PaymentMetodClientRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPaymentMetodClient;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import java.time.OffsetDateTime;

@Service
@RequiredArgsConstructor
@Log4j2
public class PaymentMetodClientImpl implements IPaymentMetodClient {
    private final ClientRepository clientRepository;
    private final OrderPaymentMethodRepository orderPaymentMethodRepository;
    private final PaymentMetodClientRepository paymentMetodClientRepository;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String namePaymentMethod, String accountDetail,String userToken, String observations) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Client client;
        OrderPaymentMethod orderPaymentMethod;
        PaymentMetodClient paymentMetodClientSave;
        try {
            user = userRepository.findByUsernameAndStatusTrue(userToken.toUpperCase());
            client = clientRepository.findByIdAndStatusTrue(user.getClientId());
            orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(namePaymentMethod.toUpperCase());
            paymentMetodClientSave = paymentMetodClientRepository.findByAccountDetailAndStatusTrue(accountDetail.toUpperCase());
        } catch (RuntimeException e) {
            throw new RuntimeException(e);
        }

        if(user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(client == null) {
            throw new BadRequestExceptions(Constants.ErrorClient);
        }

        if(orderPaymentMethod == null) {
            throw new BadRequestExceptions(Constants.ErrorPaymentMethod);
        }

        if(paymentMetodClientSave != null) {
            throw new BadRequestExceptions(Constants.ErrorPaymentDateAccountExists + " " + accountDetail.toUpperCase());
        }

        try {
            paymentMetodClientSave = paymentMetodClientRepository.save(PaymentMetodClient.builder()
                            .accountDetail(accountDetail.toUpperCase())
                            .client(client)
                            .clientId(client.getId())
                            .status(true)
                            .observations(observations)
                            .paymentMethodId(orderPaymentMethod.getId())
                            .orderPaymentMethod(orderPaymentMethod)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                    .build());

            iAudit.save("ADD_P","PEDIDO "+paymentMetodClientSave.getId()+" CREADO.",paymentMetodClientSave.getId().toString(),user.getUsername());
        } catch (RuntimeException e) {
            throw new RuntimeException(e);
        }

        return null;
    }
}
