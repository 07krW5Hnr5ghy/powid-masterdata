package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.PurchaseItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IPurchase;
import com.proyect.masterdata.services.IPurchaseItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchaseImpl implements IPurchase {
    private final PurchaseRepository purchaseRepository;
    private final UserRepository userRepository;
    private final IPurchaseItem iPurchaseItem;
    @Override
    public ResponseSuccess save(String serial, List<RequestPurchaseItem> requestPurchaseItemList, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Purchase purchase;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            purchase = purchaseRepository.findBySerial(serial.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(purchase != null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        try{
            Purchase newPurchase = purchaseRepository.save(Purchase.builder()
                            .serial(serial.toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                    .build());
            for(RequestPurchaseItem requestPurchaseItem : requestPurchaseItemList){
                iPurchaseItem.save(newPurchase.getId(),requestPurchaseItem,user.getUsername());
            }
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }
}
