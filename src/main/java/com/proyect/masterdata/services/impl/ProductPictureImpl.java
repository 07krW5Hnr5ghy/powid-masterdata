package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.ProductPicture;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ProductPictureRepository;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IFile;
import com.proyect.masterdata.services.IProductPicture;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@Service
@RequiredArgsConstructor
@Log4j2
public class ProductPictureImpl implements IProductPicture {
    private final UserRepository userRepository;
    private final IFile iFile;
    private final ProductPictureRepository productPictureRepository;
    private final ProductRepository productRepository;
    private final IAudit iAudit;
    @Override
    public List<String> uploadPicture(List<MultipartFile> pictures, Long productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Product product;
        List<String> pictureUrlList = new ArrayList<>();

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findById(productId).orElse(null);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(product == null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        try {
            String folder = (user.getClient().getBusiness() + "_PRODUCTOS").replace(" ","_");
            Date currentDate = new Date(System.currentTimeMillis());
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss");
            String dateString = dateFormat.format(currentDate);
            String formattedString = dateString.replace(" ", "_");
            String filename = "PRODUCTO_" + product.getSku() + "_" + user.getUsername() + "_" + formattedString;
            String folderPath = folder + "/" + filename;
            int pictureNumber = 1;
            if(pictures.isEmpty()){
                return Collections.emptyList();
            }
            for (MultipartFile picture : pictures){
                String url = iFile.uploadFile(picture,folderPath + "_IMAGEN_" + Integer.toString(pictureNumber)).get();
                productPictureRepository.save(ProductPicture.builder()
                        .productPictureUrl(url)
                        .product(product)
                        .productId(productId)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .registrationDate(currentDate)
                        .build());
                pictureUrlList.add(url);
                pictureNumber++;
            }
            iAudit.save("ADD_PRODUCT_PICTURE","ADD "+pictures.size()+" PRODUCT PICTURES.",user.getUsername());
            return pictureUrlList;
        }catch (RuntimeException | IOException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        } catch (ExecutionException | InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public CompletableFuture<List<String>> uploadPictureAsync(List<MultipartFile> pictures, Long productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            List<String> pictureUrlList = new ArrayList<>();

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findById(productId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try {
                String folder = (user.getClient().getBusiness() + "_PRODUCTOS").replace(" ","_");
                Date currentDate = new Date(System.currentTimeMillis());
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss");
                String dateString = dateFormat.format(currentDate);
                String formattedString = dateString.replace(" ", "_");
                String filename = "PRODUCTO_" + product.getSku() + "_" + user.getUsername() + "_" + formattedString;
                String folderPath = folder + "/" + filename;
                int pictureNumber = 1;
                if(pictures.isEmpty()){
                    return Collections.emptyList();
                }
                for (MultipartFile picture : pictures){
                    String url = iFile.uploadFile(picture,folderPath + "_IMAGEN_" + Integer.toString(pictureNumber)).get();
                    productPictureRepository.save(ProductPicture.builder()
                            .productPictureUrl(url)
                            .product(product)
                            .productId(productId)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                            .registrationDate(currentDate)
                            .build());
                    pictureUrlList.add(url);
                    pictureNumber++;
                }
                iAudit.save("ADD_PRODUCT_PICTURE","ADD "+pictures.size()+" PRODUCT PICTURES.",user.getUsername());
                return pictureUrlList;
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (ExecutionException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
