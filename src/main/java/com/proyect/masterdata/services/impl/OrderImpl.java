package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Order;
import com.proyect.masterdata.domain.OrderState;
import com.proyect.masterdata.domain.Sale;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.request.RequestItem;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestSale;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderRepository;
import com.proyect.masterdata.repository.OrderStateRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICustomer;
import com.proyect.masterdata.services.IItem;
import com.proyect.masterdata.services.IOrder;
import com.proyect.masterdata.services.ISale;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;

@RequiredArgsConstructor
@Service
@Log4j2
public class OrderImpl implements IOrder {

    private final UserRepository userRepository;
    private final OrderRepository orderRepository;
    private final OrderStateRepository orderStateRepository;
    private final ISale iSale;
    private final ICustomer iCustomer;
    private final IItem iItem;

    @Override
    public ResponseSuccess save(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        OrderState orderState;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            orderState = orderStateRepository.findByNameAndStatusTrue("PENDIENTE");
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try{

            Order order = orderRepository.save(Order.builder()
                            .cancellation(false)
                            .orderState(orderState)
                            .orderStateId(orderState.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .deliveryMan(requestOrderSave.getDeliveryMan())
                            .deliveryPhone(requestOrderSave.getDeliveryManPhone())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());

            RequestSale requestSale = RequestSale.builder()
                    .saleChannel(requestOrderSave.getSaleChannel())
                    .seller(requestOrderSave.getSeller())
                    .paymentState(requestOrderSave.getPaymentState())
                    .paymentReceipt(requestOrderSave.getPaymentReceipt())
                    .paymentMethod(requestOrderSave.getPaymentMethod())
                    .observations(requestOrderSave.getObservations())
                    .managementType(requestOrderSave.getManagementType())
                    .deliveryAmount(requestOrderSave.getDeliveryAmount())
                    .deliveryAddress(requestOrderSave.getDeliveryAddress())
                    .saleAmount(requestOrderSave.getSaleAmount())
                    .build();

            iSale.save(order,requestSale,tokenUser);

            RequestCustomer requestCustomer = RequestCustomer.builder()
                    .phone(requestOrderSave.getCustomerPhone())
                    .name(requestOrderSave.getCustomerName())
                    .type(requestOrderSave.getCustomerType())
                    .district(requestOrderSave.getCustomerDistrict())
                    .province(requestOrderSave.getCustomerProvince())
                    .department(requestOrderSave.getCustomerDepartment())
                    .instagram(requestOrderSave.getInstagram())
                    .reference(requestOrderSave.getCustomerReference())
                    .address(requestOrderSave.getCustomerAddress())
                    .build();
            
            iCustomer.save(order,requestCustomer,tokenUser);

            for(RequestItem requestItem : requestOrderSave.getRequestItems()){
                iItem.save(order,requestItem,tokenUser);
            }

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();

        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
