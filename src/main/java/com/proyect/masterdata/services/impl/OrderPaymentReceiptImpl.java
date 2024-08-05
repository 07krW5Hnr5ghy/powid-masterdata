package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderPaymentReceipt;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderPaymentReceiptRepository;
import com.proyect.masterdata.repository.OrderingRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IFile;
import com.proyect.masterdata.services.IOrderPaymentReceipt;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderPaymentReceiptImpl implements IOrderPaymentReceipt {
    private final UserRepository userRepository;
    private final IFile iFile;
    private final OrderingRepository orderingRepository;
    private final OrderPaymentReceiptRepository orderPaymentReceiptRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<List<String>> uploadReceipt(MultipartFile[] receipts, Long orderId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            List<String> receiptUrlList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            try{

                String folder = (user.getClient().getBusiness() + "_PEDIDOS").replace(" ","_");
                Date currentDate = new Date(System.currentTimeMillis());
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss");
                String dateString = dateFormat.format(currentDate);
                String formattedString = dateString.replace(" ", "_");
                String filename = "PEDIDO_" + orderId.toString() + "_" + user.getUsername() + "_" + formattedString;
                String folderPath = folder + "/" + filename;
                int receiptNumber = 1;
                if(receipts.length == 0){
                    return Collections.emptyList();
                }
                for(MultipartFile multipartFile : receipts){
                    System.out.println("Order Payment Receipt");
                    System.out.println(multipartFile.getOriginalFilename());
                    String url = iFile.uploadFile(multipartFile,folderPath + "_COMPROBANTE_" + Integer.toString(receiptNumber)).get();
                    System.out.println(url);
                    orderPaymentReceiptRepository.save(OrderPaymentReceipt.builder()
                            .paymentReceiptUrl(url)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .registrationDate(currentDate)
                            .tokenUser(user.getUsername())
                            .build());
                    receiptUrlList.add(url);
                    receiptNumber++;
                }
                iAudit.save("ADD_ORDER_PAYMENT_RECEIPT","ADD ORDER PAYMENT RECEIPT FOR ORDER "+ordering.getId()+".",user.getUsername());
                return receiptUrlList;
            }catch (RuntimeException | IOException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (ExecutionException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<List<String>> uploadReceiptAsync(MultipartFile[] receipts, Long orderId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            List<String> receiptUrlList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            try{

                String folder = (user.getClient().getBusiness() + "_PEDIDOS").replace(" ","_");
                Date currentDate = new Date(System.currentTimeMillis());
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss");
                String dateString = dateFormat.format(currentDate);
                String formattedString = dateString.replace(" ", "_");
                String filename = "PEDIDO_" + orderId.toString() + "_" + user.getUsername() + "_" + formattedString;
                String folderPath = folder + "/" + filename;
                int receiptNumber = 1;
                if(receipts.length == 0){
                    return Collections.emptyList();
                }
                for(MultipartFile multipartFile : receipts){
                    System.out.println("Order Payment Receipt");
                    System.out.println(multipartFile.getOriginalFilename());
                    String url = iFile.uploadFileAsync(multipartFile,folderPath + "_COMPROBANTE_" + Integer.toString(receiptNumber)).get();
                    System.out.println(url);
                    orderPaymentReceiptRepository.save(OrderPaymentReceipt.builder()
                            .paymentReceiptUrl(url)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .registrationDate(currentDate)
                            .tokenUser(user.getUsername())
                            .build());
                    receiptUrlList.add(url);
                    receiptNumber++;
                }
                iAudit.save("ADD_ORDER_PAYMENT_RECEIPT","ADD ORDER PAYMENT RECEIPT FOR ORDER "+ordering.getId()+".",user.getUsername());
                return receiptUrlList;
            }catch (RuntimeException | IOException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (ExecutionException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<List<String>> uploadReceiptFileAsync(List<File> fileList, Long orderId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            List<String> receiptUrlList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            try{
                String folder = (user.getClient().getBusiness() + "_PEDIDOS").replace(" ","_");
                Date currentDate = new Date(System.currentTimeMillis());
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss");
                String dateString = dateFormat.format(currentDate);
                String formattedString = dateString.replace(" ", "_");
                String filename = "PEDIDO_" + orderId.toString() + "_" + user.getUsername() + "_" + formattedString;
                String folderPath = folder + "/" + filename;
                int receiptNumber = 1;
                for(File file : fileList){
                    System.out.println("Order Payment Receipt");
                    System.out.println(file.getName());
                    String url = iFile.uploadFiles(file,folderPath + "_COMPROBANTE_" + Integer.toString(receiptNumber)).get();
                    System.out.println(url);
                    orderPaymentReceiptRepository.save(OrderPaymentReceipt.builder()
                            .paymentReceiptUrl(url)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .registrationDate(currentDate)
                            .tokenUser(user.getUsername())
                            .build());
                    receiptUrlList.add(url);
                    receiptNumber++;
                }
                iAudit.save("ADD_ORDER_PAYMENT_RECEIPT","ADD ORDER PAYMENT RECEIPT FOR ORDER "+ordering.getId()+".",user.getUsername());
                return receiptUrlList;
            }catch (RuntimeException | IOException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (ExecutionException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
