package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.StockReturn;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestStockReturn;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseRepository;
import com.proyect.masterdata.repository.StockReturnRepository;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IStockReturn;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockReturnImpl implements IStockReturn {
    private final UserRepository userRepository;
    private final PurchaseRepository purchaseRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnRepository stockReturnRepository;
    @Override
    public ResponseSuccess save(List<RequestStockReturn> requestStockReturnList, String purchaseSerial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try{
            for(RequestStockReturn requestStockReturn : requestStockReturnList){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestStockReturn.getSupplierProductSerial().toUpperCase());
                if(supplierProduct == null){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
                Purchase purchase = purchaseRepository.findBySerialAndSupplierProductId(purchaseSerial,supplierProduct.getId());
                if(purchase == null){
                    throw new BadRequestExceptions(Constants.ErrorPurchase);
                }
                if(requestStockReturn.getQuantity() > purchase.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnQuantity);
                }
                StockReturn stockReturn = stockReturnRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                if(stockReturn != null){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
                }
                stockReturnRepository.save(StockReturn.builder()
                                .client(user.getClient())
                                .clientId(user.getClientId())
                                .observations(requestStockReturn.getObservations())
                                .status(true)
                                .quantity(requestStockReturn.getQuantity())
                                .supplierProduct(supplierProduct)
                                .supplierProductId(supplierProduct.getId())
                                .registrationDate(new Date(System.currentTimeMillis()))
                                .purchase(purchase)
                                .purchaseId(purchase.getId())
                        .build());
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
