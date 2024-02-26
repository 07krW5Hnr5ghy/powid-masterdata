package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestSubscriptionPayment;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ModuleRepository;
import com.proyect.masterdata.repository.SubscriptionRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IMercadoPagoPayment;
import com.proyect.masterdata.services.ISubscriptionPayment;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class SubscriptionPaymentImpl implements ISubscriptionPayment {
    private final UserRepository userRepository;
    private final SubscriptionRepository subscriptionRepository;
    private final ModuleRepository moduleRepository;
    private final IMercadoPagoPayment iMercadoPagoPayment;
    @Override
    public String send(RequestSubscriptionPayment requestSubscriptionPayment, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Subscription subscription;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            subscription = subscriptionRepository.findByNameAndStatusTrue(requestSubscriptionPayment.getSubscriptionName().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(subscription == null){
            throw new BadRequestExceptions(Constants.ErrorSubscription);
        }

        try{

            double netAmount = 0.00;

            for(String moduleName : requestSubscriptionPayment.getModules()){
                Module module = moduleRepository.findByNameAndStatusTrue(moduleName.toUpperCase());
                if(module == null){
                    throw new BadRequestExceptions(Constants.ErrorModule);
                }
                double discount = ((module.getMonthlyPrice() * subscription.getMonths())
                        * subscription.getDiscountPercent()) / 100;
                netAmount += (module.getMonthlyPrice() * subscription.getMonths()) -
                        discount;
            }

            return iMercadoPagoPayment.sendPayment(netAmount,subscription,requestSubscriptionPayment.getModules(),user);

        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
