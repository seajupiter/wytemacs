;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   '("https://feeds.bbci.co.uk/news/world/rss.xml"
     "https://hnrss.org/newest?comments=250"
     "https://iacr.org/news/rss/"))
 '(mm-text-html-renderer 'shr)
 '(notmuch-saved-searches
   '((:name "unread" :query "tag:unread and not tag:deleted" :key [117])
     (:name "all sent" :query "tag:sent and not tag:deleted" :key
	    [115])
     (:name "all" :query "not tag:deleted" :key [97])
     (:name "all inbox" :query "tag:inbox and not tag:deleted" :key
	    [105])
     (:name "deleted" :query "tag:deleted" :key [100])
     (:name "today" :query "data:today and not tag:deleted" :key [116])
     (:name "qq inbox" :query
	    "tag:qq and tag:inbox and not tag:deleted" :key [113])
     (:name "posteo inbox" :query
	    "tag:posteo and tag:inbox and not tag:deleted" :key [112])))
 '(smtpmail-smtp-server "smtp.qq.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
