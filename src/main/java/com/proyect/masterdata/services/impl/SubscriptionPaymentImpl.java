package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.dto.request.RequestMembershipPayment;
import com.proyect.masterdata.dto.request.RequestSubscriptionPayment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IMembershipPayment;
import com.proyect.masterdata.services.IMercadoPagoPayment;
import com.proyect.masterdata.services.ISubscriptionPayment;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@Service
@RequiredArgsConstructor
@Log4j2
public class SubscriptionPaymentImpl implements ISubscriptionPayment {
    private final UserRepository userRepository;
    private final SubscriptionRepository subscriptionRepository;
    private final ModuleRepository moduleRepository;
    private final IMercadoPagoPayment iMercadoPagoPayment;
    private final MembershipStateRepository membershipStateRepository;
    private final MembershipRepository membershipRepository;
    private final IMembershipPayment iMembershipPayment;
    private final IAudit iAudit;
    private final PaymentGatewayRepository paymentGatewayRepository;
    @Override
    public CompletableFuture<String> send(RequestSubscriptionPayment requestSubscriptionPayment, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Subscription subscription;
            MembershipState payedState;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                subscription = subscriptionRepository.findByNameAndStatusTrue(requestSubscriptionPayment.getSubscriptionName().toUpperCase());
                payedState = membershipStateRepository.findByNameAndStatusTrue("PAGADA");
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

                Membership payedMembership = membershipRepository.findByClientIdAndMembershipStateId(user.getId(), payedState.getId());
                if(payedMembership != null){
                    throw new BadRequestExceptions(Constants.ErrorMembershipActivePayed);
                }

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

                iAudit.save("SEND_SUBSCRIPTION_PAYMENT","PAGO DE SUBSCRIPCION "+subscription.getName()+" ENVIADO.",user.getClient().getRuc(),user.getUsername());

                return iMercadoPagoPayment.sendPayment(netAmount,subscription,requestSubscriptionPayment.getModules(),user).get();

            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (ExecutionException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activateDemo(String username) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Membership demoMembership;
            PaymentGateway paymentGateway;

            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                paymentGateway = paymentGatewayRepository.findByNameAndStatusTrue("DEMO");
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                demoMembership = membershipRepository.findByClientIdAndDemoTrue(user.getClientId());
            }

            if(paymentGateway==null){
                throw new BadRequestExceptions(Constants.ErrorPaymentGateway);
            }

            if(demoMembership != null){
                throw new BadRequestExceptions(Constants.ErrorMembershipDemo);
            }

            try{
                List<String> moduleNames = new ArrayList<>();
                moduleNames.add("MÓDULO DE VENTAS");
                moduleNames.add("MÓDULO DE GESTIÓN");
                moduleNames.add("MÓDULO DE ALMACÉN");
                RequestSubscriptionPayment.builder()
                        .paymentGateway("DEMO")
                        .subscriptionName("MENSUAL")
                        .modules(moduleNames)
                        .build();

                iMembershipPayment.save(RequestMembershipPayment.builder()
                        .paymentGateway("DEMO")
                        .demo(true)
                        .modules(moduleNames)
                        .subscriptionName("MENSUAL")
                        .grossAmount(0.00)
                        .paymentGatewayFee(0.00)
                        .netAmount(0.00)
                        .taxAmount(0.00)
                        .build(), user.getUsername());
                iAudit.save("ACTIVATE_DEMO_ACCOUNT","CUENTA DEMO HABILITADA PARA CLIENTE CON RUC "+user.getClient().getRuc()+".",user.getClient().getRuc(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
