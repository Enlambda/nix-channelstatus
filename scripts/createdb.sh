dropdb channelstatus || true
createdb channelstatus
cat scripts/schema.sql | psql  channelstatus
#stack exec opaleye-gen -- -d postgresql:///channelstatus -o src/ChannelStatus/Database.hs
