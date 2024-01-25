package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Item;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ItemRepository;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class ItemImpl implements IItem {
    private final UserRepository userRepository;
    private final ItemRepository itemRepository;
    private final ProductRepository productRepository;
    @Override
    public ResponseSuccess save(RequestItem requestItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Product product;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findBySkuAndStatusTrue(requestItem.getProductSku());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (product == null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        try{
            itemRepository.save(Item.builder()
                            .discount(requestItem.getDiscount())
                            .order(requestItem.getOrder())
                            .orderId(requestItem.getOrder().getId())
                            .quantity(requestItem.getQuantity())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .product(product)
                            .productId(product.getId())
                            .tokenUser(user.getUsername())
                    .build());
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
